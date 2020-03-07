{-# LANGUAGE RankNTypes #-}
module Control.Effect.Parser.Lens
( Lens'
) where

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)
