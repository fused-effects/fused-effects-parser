{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Parser.Church
( -- * Parser carrier
  parseString
, parseFile
, parseInput
, runParser
, Input(..)
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
import Data.Coerce (coerce)
import Data.Functor.Compose
import Data.Functor.Identity
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
parseInput path pos input m = runReader (Lines inputLines) (runReader (Path path) (runParserC m success failure failure (Input pos input)))
  where
  success _ a = pure (Right a)
  failure (Input pos _) reason = pure (Left (Notice (Just Error) (Excerpt path (inputLines !! Span.line pos) (Span pos pos)) (fromMaybe (pretty "unknown error") reason) []))
  inputLines = lines input
  lines "" = [""]
  lines s  = let (line, rest) = takeLine s in line : lines rest
  takeLine ""          = ("", "")
  takeLine ('\n':rest) = ("\n", rest)
  takeLine (c   :rest) = let (cs, rest') = takeLine rest in (c:cs, rest')

runParser
  :: (Input -> a -> m r)
  -> (Input -> Maybe (Doc AnsiStyle) -> m r)
  -> (Input -> Maybe (Doc AnsiStyle) -> m r)
  -> Input
  -> ParserC m a
  -> m r
runParser just nothing fail input (ParserC run) = run just nothing fail input

data Input = Input
  { pos :: {-# UNPACK #-} !Pos
  , str :: !String
  }
  deriving (Eq, Ord, Show)

newtype ParserC m a = ParserC
  { runParserC
    :: forall r
    .  (Input -> a -> m r)                     -- success
    -> (Input -> Maybe (Doc AnsiStyle) -> m r) -- empty
    -> (Input -> Maybe (Doc AnsiStyle) -> m r) -- cut
    -> Input
    -> m r
  }
  deriving (Functor)

instance Applicative (ParserC m) where
  pure a = ParserC (\ just _ _ input -> just input a)
  (<*>) = ap

instance Alternative (ParserC m) where
  empty = ParserC (\ _ nothing _ input -> nothing input Nothing)

  ParserC l <|> ParserC r = ParserC (\ just nothing fail input -> l just (const (const (r just nothing fail input))) fail input)

instance Monad (ParserC m) where
  ParserC m >>= f = ParserC (\ just nothing fail -> m (\ input -> runParser just nothing fail input . f) nothing fail)

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
      Accept p k   -> ParserC (\ just nothing _ input -> case str input of
        c:_ | Just a <- p c -> just (advance input) a
            | otherwise     -> nothing input (Just (pretty "unexpected " <> pretty c))
        _                   -> nothing input (Just (pretty "unexpected EOF"))) >>= k
      Label m s k  -> ParserC (\ just nothing fail -> runParserC m just (\ p r -> nothing p (r <|> Just (pretty s))) (\ p r -> fail p (r <|> Just (pretty s)))) >>= k
      Unexpected s -> ParserC $ \ _ nothing _ input -> nothing input (Just (pretty s))
      Position k   -> ParserC (\ just _ _ input -> just input (pos input)) >>= k

    R (L cut) -> case cut of
      Cutfail  -> ParserC $ \ _ _ fail input -> fail input Nothing
      Call m k -> ParserC (\ just nothing _ -> runParserC m just nothing nothing) >>= k

    R (R (L nondet)) -> case nondet of
      L Empty      -> empty
      R (Choose k) -> k True <|> k False

    R (R (R other)) -> ParserC $ \ just nothing fail input -> do
      res <- alg (thread (Compose (input, pure ())) dst other)
      runIdentity (uncurry (runParser (coerce just) (coerce nothing) (coerce fail)) (getCompose res))
    where
    dst :: Compose ((,) Input) (ParserC Identity) (ParserC m a) -> m (Compose ((,) Input) (ParserC Identity) a)
    dst = runIdentity
        . uncurry (runParser
          (fmap pure . runParser
            (\ i a -> pure (Compose (i, pure a)))
            (\ i _ -> pure (Compose (i, empty)))
            (\ i _ -> pure (Compose (i, cutfail))))
          (\ i _ -> pure (pure (Compose (i, empty))))
          (\ i _ -> pure (pure (Compose (i, cutfail)))))
        . getCompose


data Result a = Result
  { resultInput :: {-# UNPACK #-} !Input
  , resultState :: Either (Maybe (Doc AnsiStyle)) a
  }
  deriving (Foldable, Functor, Show, Traversable)

success :: Input -> a -> Result a
success i a = Result i (Right a)


advance :: Input -> Input
advance (Input pos (c:cs)) = Input (advancePos c pos) cs
advance i                  = i

advancePos :: Char -> Pos -> Pos
advancePos '\n' p = Pos (succ (Span.line p)) 0
advancePos _    p = p { Span.column = succ (Span.column p) }
