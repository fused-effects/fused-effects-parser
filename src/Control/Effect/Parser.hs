{-# LANGUAGE GADTs #-}
module Control.Effect.Parser
( -- * Parser effect
  Parser(..)
, accept
, position
, spanned
, source
, line
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Algebra
import qualified Control.Effect.Parser.Source as Source
import qualified Control.Effect.Parser.Span as Span

data Parser m k where
  Accept     :: (Char -> Maybe a) -> Parser m a
  Label      :: m a -> String ->     Parser m a
  Unexpected :: String ->            Parser m a
  Position   ::                      Parser m Span.Pos
  Source     ::                      Parser m Source.Source


accept :: Has Parser sig m => (Char -> Maybe a) -> m a
accept p = send (Accept p)
{-# INLINE accept #-}

position :: Has Parser sig m => m Span.Pos
position = send Position
{-# INLINE position #-}

spanned :: Has Parser sig m => m a -> m (Span.Span, a)
spanned m = do
  start <- position
  a <- m
  end <- position
  pure (Span.Span start end, a)
{-# INLINE spanned #-}

source :: Has Parser sig m => m Source.Source
source = send Source
{-# INLINE source #-}

line :: Has Parser sig m => m Source.Line
line = do
  pos <- position
  source <- source
  pure (source Source.! pos)
{-# INLINE line #-}
