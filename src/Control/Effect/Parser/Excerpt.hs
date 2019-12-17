{-# LANGUAGE DeriveTraversable #-}
module Control.Effect.Parser.Excerpt
( Excerpt(..)
, Excerpted(..)
, unExcerpted
) where

import qualified Source.Span as Span

data Excerpt = Excerpt
  { path :: !FilePath
  , line :: !String
  , span :: {-# UNPACK #-} !Span.Span
  }
  deriving (Eq, Ord, Show)

instance Semigroup Excerpt where
  Excerpt _ l s1 <> Excerpt p _ s2 = Excerpt p l (s1 <> s2)


data Excerpted a = a :~ Excerpt
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

unExcerpted :: Excerpted a -> a
unExcerpted (a :~ _) = a
