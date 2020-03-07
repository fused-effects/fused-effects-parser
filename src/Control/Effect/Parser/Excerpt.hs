{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Excerpt
( Excerpt(..)
, Excerpted(..)
, getExcerpted
, excerpted
) where

import           Control.Effect.Parser as Parser
import           Control.Effect.Parser.Lens
import           Control.Effect.Parser.Lines as Parser
import           Control.Effect.Parser.Path as Parser
import           Control.Effect.Reader
import           Prelude hiding (span)
import qualified Source.Span as Span

data Excerpt = Excerpt
  { path :: !Path
  , line :: !String
  , span :: {-# UNPACK #-} !Span.Span
  }
  deriving (Eq, Ord, Show)

instance Span.HasSpan Excerpt where
  span_ = lens span $ \ e span -> e{ span }
  {-# INLINE span_ #-}

instance Semigroup Excerpt where
  Excerpt _ l s1 <> Excerpt p _ s2 = Excerpt p l (s1 <> s2)
  {-# INLINE (<>) #-}


data Excerpted a = a :~ Excerpt
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

getExcerpted :: Excerpted a -> a
getExcerpted (a :~ _) = a
{-# INLINE getExcerpted #-}

excerpted :: (Has Parser sig m, Has (Reader Lines) sig m, Has (Reader Path) sig m) => m a -> m (Excerpted a)
excerpted m = do
  path <- Parser.path
  line <- Parser.line
  start <- position
  a <- m
  end <- position
  pure (a :~ Excerpt path line (Span.Span start end))
{-# INLINE excerpted #-}
