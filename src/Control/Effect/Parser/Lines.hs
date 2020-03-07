{-# LANGUAGE LambdaCase #-}
module Control.Effect.Parser.Lines
( Lines(..)
, linesFromString
, line
) where

import           Control.Effect.Parser
import           Control.Effect.Reader
import qualified Source.Span as Span

newtype Lines = Lines { getLines :: [String] }
  deriving (Eq, Ord, Show)

linesFromString :: String -> Lines
linesFromString = Lines . go
  where
  go = \case
    "" -> [""]
    s  -> let (line, rest) = takeLine s in line : go rest
{-# INLINE linesFromString #-}

takeLine :: String -> (String, String)
takeLine = go id where
  go line = \case
    ""        -> (line "", "")
    '\n':rest -> (line "\n", rest)
    c   :rest -> go (line . (c:)) rest
{-# INLINE takeLine #-}


line :: (Has Parser sig m, Has (Reader Lines) sig m) => m String
line = do
  pos <- position
  asks ((!! Span.line pos) . getLines)
{-# INLINE line #-}
