{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Excerpt
( Excerpt(..)
, path_
, line_
, spanned
, excerpted
) where

import qualified Control.Effect.Parser as Parser
import           Control.Effect.Parser.Lens
import qualified Control.Effect.Parser.Source as Source
import qualified Control.Effect.Parser.Span as Span
import           Control.Effect.Reader
import           Prelude hiding (span)

data Excerpt = Excerpt
  { path :: !(Maybe FilePath)
  , line :: !Source.Line
  , span :: {-# UNPACK #-} !Span.Span
  }
  deriving (Eq, Ord, Show)

instance Semigroup Excerpt where
  Excerpt _ l s1 <> Excerpt p _ s2 = Excerpt p l (s1 <> s2)
  {-# INLINE (<>) #-}

path_ :: Lens' Excerpt (Maybe FilePath)
path_ = lens path $ \ e path -> e{ path }
{-# INLINE path_ #-}

line_ :: Lens' Excerpt Source.Line
line_ = lens line $ \ e line -> e{ line }
{-# INLINE line_ #-}


spanned :: Has Parser.Parser sig m => m a -> m (Span.Span, a)
spanned m = do
  start <- Parser.position
  a <- m
  end <- Parser.position
  pure (Span.Span start end, a)
{-# INLINE spanned #-}

excerpted :: Has Parser.Parser sig m => m a -> m (Excerpt, a)
excerpted m = do
  Source.Source path lines <- Parser.source
  (span, a) <- spanned m
  pure (Excerpt path (lines !! Span.line (Span.start span)) span, a)
{-# INLINE excerpted #-}
