{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Excerpt
( Excerpt(..)
, fromSourceAndSpan
, path_
, lines_
, span_
) where

import           Control.Effect.Parser.Lens
import qualified Control.Effect.Parser.Source as Source
import qualified Control.Effect.Parser.Span as Span
import qualified Data.List.NonEmpty as NE
import           Prelude hiding (lines, span)

data Excerpt = Excerpt
  { path  :: !(Maybe FilePath)
  , lines :: !(NE.NonEmpty Source.Line)
  , span  :: {-# UNPACK #-} !Span.Span
  }
  deriving (Eq, Ord, Show)

instance Semigroup Excerpt where
  Excerpt _ l s1 <> Excerpt p _ s2 = Excerpt p l (s1 <> s2)
  {-# INLINE (<>) #-}

fromSourceAndSpan :: Source.Source -> Span.Span -> Excerpt
fromSourceAndSpan src span = Excerpt{ path = Source.path src, lines = src Source.!.. span, span }
{-# INLINABLE fromSourceAndSpan #-}


path_ :: Lens' Excerpt (Maybe FilePath)
path_ = lens path $ \ e path -> e{ path }
{-# INLINE path_ #-}

lines_ :: Lens' Excerpt (NE.NonEmpty Source.Line)
lines_ = lens lines $ \ e lines -> e{ lines }
{-# INLINE lines_ #-}

span_ :: Lens' Excerpt Span.Span
span_ = lens span $ \ e span -> e{ span }
{-# INLINE span_ #-}
