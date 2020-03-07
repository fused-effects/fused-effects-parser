{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Parser
( -- * Parser effect
  Parser(..)
, accept
, position
, Lines(..)
, linesFromString
, line
, Path(..)
, path
, excerpted
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Algebra
import           Control.Effect.Parser.Excerpt (Excerpt(Excerpt), Excerpted(..))
import           Control.Effect.Reader
import           Prelude hiding (lines)
import qualified Source.Span as Span

data Parser m k
  = forall a . Accept (Char -> Maybe a) (a -> m k)
  | forall a . Label (m a) String (a -> m k)
  | Unexpected String
  | Position (Span.Pos -> m k)

deriving instance Functor m => Functor (Parser m)

instance HFunctor Parser where
  hmap f = \case
    Accept p   k -> Accept p      (f . k)
    Label m s  k -> Label (f m) s (f . k)
    Unexpected s -> Unexpected s
    Position   k -> Position      (f . k)

instance Effect Parser where
  thread ctx hdl = \case
    Accept p   k -> Accept p (hdl . (<$ ctx) . k)
    Label m s  k -> Label (hdl (m <$ ctx)) s (hdl . fmap k)
    Unexpected s -> Unexpected s
    Position   k -> Position (hdl . (<$ ctx) . k)


accept :: Has Parser sig m => (Char -> Maybe a) -> m a
accept p = send (Accept p pure)

position :: Has Parser sig m => m Span.Pos
position = send (Position pure)


newtype Lines = Lines { getLines :: [String] }

linesFromString :: String -> Lines
linesFromString = Lines . go
  where
  go = \case
    "" -> [""]
    s  -> let (line, rest) = takeLine s in line : go rest

takeLine :: String -> (String, String)
takeLine = go id where
  go line = \case
    ""        -> (line "", "")
    '\n':rest -> (line "\n", rest)
    c   :rest -> go (line . (c:)) rest

line :: (Has Parser sig m, Has (Reader Lines) sig m) => m String
line = do
  pos <- position
  asks ((!! Span.line pos) . getLines)


newtype Path = Path { getPath :: FilePath }
  deriving (Eq, Ord, Show)

path :: Has (Reader Path) sig m => m FilePath
path = asks getPath


excerpted :: (Has Parser sig m, Has (Reader Lines) sig m, Has (Reader Path) sig m) => m a -> m (Excerpted a)
excerpted m = do
  path <- path
  line <- line
  start <- position
  a <- m
  end <- position
  pure (a :~ Excerpt path line (Span.Span start end))
