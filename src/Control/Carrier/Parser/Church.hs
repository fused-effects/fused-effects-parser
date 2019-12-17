{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Parser.Church
( -- * Parser carrier
  parseString
, parseFile
, runParser
, ParserC(..)
, Level(..)
, prettyLevel
, Notice(..)
, prettyNotice
  -- * Parser effect
, module Control.Effect.Parser
) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Cut
import Control.Effect.Error
import Control.Effect.NonDet
import Control.Effect.Parser
import Control.Monad (ap)
import Control.Monad.IO.Class
import Data.Foldable (fold)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color(..), color)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import Source.Span as Span
import Text.Parser.Char (CharParsing(..))
import Text.Parser.Combinators
import Text.Parser.Token (TokenParsing)

parseString :: Has (Error Notice) sig m => ParserC (ReaderC Path (ReaderC Lines m)) a -> Pos -> String -> m a
parseString p pos input = runParser "(interactive)" pos input p >>= either throwError pure

parseFile :: (Has (Error Notice) sig m, MonadIO m) => ParserC (ReaderC Path (ReaderC Lines m)) a -> FilePath -> m a
parseFile p path = do
  input <- liftIO (readFile path)
  runParser path (Pos 0 0) input p >>= either throwError pure

runParser :: Applicative m => FilePath -> Pos -> String -> ParserC (ReaderC Path (ReaderC Lines m)) a -> m (Either Notice a)
runParser path pos input m = runReader (Lines inputLines) (runReader (Path path) (runParserC m success failure failure pos input)) where
  success _ _ a = pure (Right a)
  failure pos reason = pure (Left (Notice (Just Error) (Excerpt path (inputLines !! Span.line pos) (Span pos pos)) (fromMaybe (pretty "unknown error") reason) []))
  inputLines = lines input
  lines "" = [""]
  lines s  = let (line, rest) = takeLine s in line : lines rest
  takeLine ""          = ("", "")
  takeLine ('\n':rest) = ("\n", rest)
  takeLine (c   :rest) = let (cs, rest') = takeLine rest in (c:cs, rest')

newtype ParserC m a = ParserC
  { runParserC
    :: forall r
    .  (Pos -> String -> a -> m r)           -- success
    -> (Pos -> Maybe (Doc AnsiStyle) -> m r) -- empty
    -> (Pos -> Maybe (Doc AnsiStyle) -> m r) -- cut
    -> Pos
    -> String
    -> m r
  }
  deriving (Functor)

instance Applicative (ParserC m) where
  pure a = ParserC (\ just _ _ pos input -> just pos input a)
  (<*>) = ap

instance Alternative (ParserC m) where
  empty = ParserC (\ _ nothing _ pos _ -> nothing pos Nothing)

  ParserC l <|> ParserC r = ParserC (\ just nothing fail pos input -> l just (const (const (r just nothing fail pos input))) fail pos input)

instance Monad (ParserC m) where
  m >>= f = ParserC (\ just nothing fail -> runParserC m (\ pos input a -> runParserC (f a) just nothing fail pos input) nothing fail)

instance MonadPlus (ParserC m)

instance (Algebra sig m, Effect sig) => Parsing (ParserC m) where
  try = call
  eof = notFollowedBy anyChar <?> "end of input"
  unexpected s = send (Unexpected s)
  m <?> s = send (Label m s pure)
  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))

instance (Algebra sig m, Effect sig) => CharParsing (ParserC m) where
  satisfy p = accept (\ c -> if p c then Just c else Nothing)

instance (Algebra sig m, Effect sig) => TokenParsing (ParserC m)

instance (Algebra sig m, Effect sig) => Algebra (Parser :+: Cut :+: NonDet :+: sig) (ParserC m) where
  alg = \case
    L parser -> case parser of
      Accept p k -> ParserC (\ just nothing _ pos input -> case input of
        c:cs | Just a <- p c -> just (advancePos c pos) cs a
             | otherwise     -> nothing pos (Just (pretty "unexpected " <> pretty c))
        _                    -> nothing pos (Just (pretty "unexpected EOF"))) >>= k
      Label m s k -> ParserC (\ just nothing fail -> runParserC m just (\ p r -> nothing p (r <|> Just (pretty s))) (\ p r -> fail p (r <|> Just (pretty s)))) >>= k
      Unexpected s -> ParserC $ \ _ nothing _ pos _ -> nothing pos (Just (pretty s))
      Position k -> ParserC (\ just _ _ pos input -> just pos input pos) >>= k
    R (L cut) -> case cut of
      Cutfail -> ParserC $ \ _ _ fail pos _ -> fail pos Nothing
      Call m k -> ParserC (\ just nothing _ -> runParserC m just nothing nothing) >>= k
    R (R (L nondet)) -> case nondet of
      L Empty      -> empty
      R (Choose k) -> k True <|> k False
    R (R (R other)) -> ParserC $ \ just nothing _ pos input -> alg (thread (success pos input ()) (result runParser failure) other) >>= result just nothing where
      runParser p s m = runParserC m (\ p s -> pure . success p s) failure failure p s
      success pos input a = Result pos (Right (input, a))
      failure pos reason = pure (Result pos (Left reason))


data Result a = Result
  { resultPos   :: {-# UNPACK #-} !Pos
  , resultState :: Either (Maybe (Doc AnsiStyle)) (String, a)
  }
  deriving (Foldable, Functor, Show, Traversable)

result :: (Pos -> String -> a -> b) -> (Pos -> Maybe (Doc AnsiStyle) -> b) -> Result a -> b
result success failure (Result pos state) = either (failure pos) (uncurry (success pos)) state


advancePos :: Char -> Pos -> Pos
advancePos '\n' p = Pos (succ (Span.line p)) 0
advancePos _    p = p { Span.column = succ (Span.column p) }


data Level
  = Warn
  | Error
  deriving (Eq, Ord, Show)

prettyLevel :: Level -> Doc AnsiStyle
prettyLevel = \case
  Warn  -> magenta (pretty "warning")
  Error -> red     (pretty "error")


data Notice = Notice
  { noticeLevel   :: Maybe Level
  , noticeExcerpt :: {-# UNPACK #-} !Excerpt
  , noticeReason  :: Doc AnsiStyle
  , noticeContext :: [Doc AnsiStyle]
  }
  deriving (Show)

prettyNotice :: Notice -> Doc AnsiStyle
prettyNotice (Notice level (Excerpt path line span) reason context) = vsep
  ( nest 2 (group (vsep [bold (pretty path) <> colon <> bold (pretty (succ (Span.line (Span.start span)))) <> colon <> bold (pretty (succ (Span.column (Span.start span)))) <> colon <> maybe mempty ((space <>) . (<> colon) . prettyLevel) level, reason]))
  : blue (pretty (succ (Span.line (Span.start span)))) <+> align (fold
    [ blue (pretty '|') <+> pretty line <> if "\n" `isSuffixOf` line then mempty else blue (pretty "<EOF>") <> hardline
    , blue (pretty '|') <+> caret span
    ])
  : context) where
  caret span = pretty (replicate (Span.column (Span.start span)) ' ') <> prettySpan span

  prettySpan (Span start end)
    | start == end                     = green (pretty '^')
    | Span.line start == Span.line end = green (pretty (replicate (Span.column end - Span.column start) '~'))
    | otherwise                        = green (pretty "^…")

  bold = annotate ANSI.bold


red, green, blue, magenta :: Doc AnsiStyle -> Doc AnsiStyle
red     = annotate $ color Red
green   = annotate $ color Green
blue    = annotate $ color Blue
magenta = annotate $ color Magenta
