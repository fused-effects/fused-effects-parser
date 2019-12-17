{-# LANGUAGE DeriveFunctor, ExistentialQuantification, LambdaCase, StandaloneDeriving #-}
module Control.Effect.Parser
( -- * Parser effect
  Parser(..)
, accept
, position
, Lines(..)
, line
, Path(..)
, path
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Reader
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


newtype Lines = Lines { unLines :: [String] }

line :: (Has Parser sig m, Has (Reader Lines) sig m) => m String
line = do
  pos <- position
  asks ((!! Span.line pos) . unLines)


newtype Path = Path { unPath :: FilePath }

path :: Has (Reader Path) sig m => m FilePath
path = asks unPath
