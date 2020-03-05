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
import qualified Source.Span as Span

data Parser m k
  = forall a . Accept (Char -> Maybe a) (a -> m k)
  | forall a . Label (m a) String (a -> m k)
  | Unexpected String
  | Position (Span.Pos -> m k)

deriving instance Functor m => Functor (Parser m)


accept :: Has Parser sig m => (Char -> Maybe a) -> m a
accept p = send (Accept p pure)

position :: Has Parser sig m => m Span.Pos
position = send (Position pure)


newtype Lines = Lines { unLines :: [String] }

line :: (Has Parser sig m, Has (Reader Lines) sig m) => m String
line = do
  pos <- position
  asks ((!! Span.line pos) . unLines)


newtype Path = Path { unPath :: FilePath }
  deriving (Eq, Ord, Show)

path :: Has (Reader Path) sig m => m FilePath
path = asks unPath


excerpted :: (Has Parser sig m, Has (Reader Lines) sig m, Has (Reader Path) sig m) => m a -> m (Excerpted a)
excerpted m = do
  path <- path
  line <- line
  start <- position
  a <- m
  end <- position
  pure (a :~ Excerpt path line (Span.Span start end))
