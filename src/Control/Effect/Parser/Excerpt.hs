{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Excerpt
( Excerpt(..)
, path_
, line_
, spanned
, excerpted
) where

import           Control.Effect.Parser as Parser
import           Control.Effect.Parser.Lens
import qualified Control.Effect.Parser.Lines as Parser
import           Control.Effect.Parser.Path (Path)
import qualified Control.Effect.Parser.Path as Parser
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

path_ :: Lens' Excerpt Path
path_ = lens path $ \ e path -> e{ path }
{-# INLINE path_ #-}

line_ :: Lens' Excerpt String
line_ = lens line $ \ e line -> e{ line }
{-# INLINE line_ #-}


spanned :: Has Parser sig m => m a -> m (Span.Span, a)
spanned m = do
  start <- position
  a <- m
  end <- position
  pure (Span.Span start end, a)
{-# INLINE spanned #-}

excerpted :: (Has Parser sig m, Has (Reader Parser.Lines) sig m, Has (Reader Path) sig m) => m a -> m (Excerpt, a)
excerpted m = do
  path <- Parser.path
  line <- Parser.line
  (span, a) <- spanned m
  pure (Excerpt path line span, a)
{-# INLINE excerpted #-}
