{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Parser
( -- * Parser effect
  Parser(..)
, accept
, position
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Algebra
import qualified Source.Span as Span

data Parser m k
  = forall a . Accept (Char -> Maybe a) (a -> m k)
  | forall a . Label (m a) String (a -> m k)
  | Unexpected String
  | Position (Span.Pos -> m k)

deriving instance Functor m => Functor (Parser m)


accept :: Has Parser sig m => (Char -> Maybe a) -> m a
accept p = send (Accept p pure)
{-# INLINE accept #-}

position :: Has Parser sig m => m Span.Pos
position = send (Position pure)
{-# INLINE position #-}
