{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Excerpt
( Excerpt(..)
, fromSourceAndSpan
, path_
, line_
, span_
) where

import           Control.Effect.Parser.Lens
import qualified Control.Effect.Parser.Source as Source
import qualified Control.Effect.Parser.Span as Span
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

fromSourceAndSpan :: Source.Source -> Span.Span -> Excerpt
fromSourceAndSpan src span = Excerpt{ path = Source.path src, line = src Source.! Span.start span, span }
{-# INLINABLE fromSourceAndSpan #-}


path_ :: Lens' Excerpt (Maybe FilePath)
path_ = lens path $ \ e path -> e{ path }
{-# INLINE path_ #-}

line_ :: Lens' Excerpt Source.Line
line_ = lens line $ \ e line -> e{ line }
{-# INLINE line_ #-}

span_ :: Lens' Excerpt Span.Span
span_ = lens span $ \ e span -> e{ span }
{-# INLINE span_ #-}
