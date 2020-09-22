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
import qualified Control.Effect.Parser.Source as Source
import           Control.Effect.Reader
import           Prelude hiding (span)
import qualified Source.Span as Span

data Excerpt = Excerpt
  { path :: !(Maybe FilePath)
  , line :: !Source.Line
  , span :: {-# UNPACK #-} !Span.Span
  }
  deriving (Eq, Ord, Show)

instance Span.HasSpan Excerpt where
  span_ = lens span $ \ e span -> e{ span }
  {-# INLINE span_ #-}

instance Semigroup Excerpt where
  Excerpt _ l s1 <> Excerpt p _ s2 = Excerpt p l (s1 <> s2)
  {-# INLINE (<>) #-}

path_ :: Lens' Excerpt (Maybe FilePath)
path_ = lens path $ \ e path -> e{ path }
{-# INLINE path_ #-}

line_ :: Lens' Excerpt Source.Line
line_ = lens line $ \ e line -> e{ line }
{-# INLINE line_ #-}


spanned :: Has Parser sig m => m a -> m (Span.Span, a)
spanned m = do
  start <- position
  a <- m
  end <- position
  pure (Span.Span start end, a)
{-# INLINE spanned #-}

excerpted :: (Has Parser sig m, Has (Reader Source.Source) sig m) => m a -> m (Excerpt, a)
excerpted m = do
  Source.Source path lines <- Source.source
  (span, a) <- spanned m
  pure (Excerpt path (lines !! Span.line (Span.start span)) span, a)
{-# INLINE excerpted #-}
