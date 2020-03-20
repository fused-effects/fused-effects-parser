{-# LANGUAGE LambdaCase #-}
module Control.Effect.Parser.Lines
( Lines(..)
, linesFromString
, line
, (!)
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
    s  -> let (line, rest) = takeLine s in line : either (const []) go rest
{-# INLINE linesFromString #-}

takeLine :: String -> (String, Either String String)
takeLine = go id where
  go line = \case
    ""        -> (line "", Left "")
    '\r':rest -> (line "\r", Right rest)
    '\n':rest -> (line "\n", Right rest)
    c   :rest -> go (line . (c:)) rest
{-# INLINE takeLine #-}

(!) :: Lines -> Span.Pos -> String
Lines lines ! pos = lines !! Span.line pos
{-# INLINE (!) #-}

infixl 9 !


line :: (Has Parser sig m, Has (Reader Lines) sig m) => m String
line = do
  pos <- position
  asks (! pos)
{-# INLINE line #-}
