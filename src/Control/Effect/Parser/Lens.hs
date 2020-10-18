{-# LANGUAGE RankNTypes #-}
module Control.Effect.Parser.Lens
( Lens
, Lens'
, lens
, (.~)
, (%~)
, (&)
) where

import Data.Coerce (coerce)
import Data.Function
import Data.Functor.Identity

type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)
type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens get put afa s = fmap (put s) (afa (get s))
{-# INLINE lens #-}

(.~) :: Lens' s a -> a -> s -> s
l .~ b = coerce . l (const (Identity b))
{-# INLINE (.~) #-}

infixr 4 .~

(%~) :: Lens' s a -> (a -> a) -> s -> s
l %~ f = runIdentity . l (Identity . f)
{-# INLINE (%~) #-}

infixr 4 %~
