{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Parser.Church
( -- * Parser carrier
  parseString
, parseFile
, parseInput
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
import Control.Effect.NonDet
import Control.Effect.Parser
import Control.Effect.Parser.Excerpt
import Control.Effect.Parser.Notice
import Control.Effect.Throw
import Control.Monad (ap)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Source.Span as Span
import Text.Parser.Char (CharParsing(..))
import Text.Parser.Combinators
import Text.Parser.Token (TokenParsing)

parseString :: Has (Throw Notice) sig m => Pos -> String -> ParserC (ReaderC Path (ReaderC Lines m)) a -> m a
parseString pos input p = parseInput "(interactive)" pos input p >>= either throwError pure

parseFile :: (Has (Throw Notice) sig m, MonadIO m) => FilePath -> ParserC (ReaderC Path (ReaderC Lines m)) a -> m a
parseFile path p = do
  input <- liftIO (readFile path)
  parseInput path (Pos 0 0) input p >>= either throwError pure

parseInput :: Applicative m => FilePath -> Pos -> String -> ParserC (ReaderC Path (ReaderC Lines m)) a -> m (Either Notice a)
parseInput path pos input m = runReader (Lines inputLines) (runReader (Path path) (runParserC m success failure failure pos input))
  where
  success _ _ a = pure (Right a)
  failure pos reason = pure (Left (Notice (Just Error) (Excerpt path (inputLines !! Span.line pos) (Span pos pos)) (fromMaybe (pretty "unknown error") reason) []))
  inputLines = lines input
  lines "" = [""]
  lines s  = let (line, rest) = takeLine s in line : lines rest
  takeLine ""          = ("", "")
  takeLine ('\n':rest) = ("\n", rest)
  takeLine (c   :rest) = let (cs, rest') = takeLine rest in (c:cs, rest')

runParser
  :: (Pos -> String -> a -> m r)
  -> (Pos -> Maybe (Doc AnsiStyle) -> m r)
  -> (Pos -> Maybe (Doc AnsiStyle) -> m r)
  -> Pos
  -> String
  -> ParserC m a
  -> m r
runParser just nothing fail pos input (ParserC run) = run just nothing fail pos input

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
    R (R (R other)) -> ParserC $ \ just nothing _ pos input -> alg (thread (success pos input ()) (result (runParser (\ p s -> pure . success p s) failure failure) failure) other) >>= result just nothing where
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
