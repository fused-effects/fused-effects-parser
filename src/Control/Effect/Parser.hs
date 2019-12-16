{-# LANGUAGE DeriveFunctor, ExistentialQuantification, LambdaCase, StandaloneDeriving #-}
module Control.Effect.Parser
( -- * Re-exports
  Algebra
, Has
, run
) where

import Control.Algebra
import Source.Span

data Parser m k
  = forall a . Accept (Char -> Maybe a) (a -> m k)
  | forall a . Label (m a) String (a -> m k)
  | Unexpected String
  | Position (Pos -> m k)

deriving instance Functor m => Functor (Parser m)

instance HFunctor Parser where
  hmap f = \case
    Accept p   k -> Accept p      (f . k)
    Label m s  k -> Label (f m) s (f . k)
    Unexpected s -> Unexpected s
    Position   k -> Position      (f . k)
