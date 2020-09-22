{-# LANGUAGE LambdaCase #-}
module Control.Effect.Parser.Lines
( Lines(..)
, Line(..)
, LineEnding(..)
, linesFromString
, line
, (!)
) where

import           Control.Effect.Parser
import           Control.Effect.Reader
import qualified Source.Span as Span

newtype Lines = Lines { getLines :: [Line] }
  deriving (Eq, Ord, Show)

data Line = Line String LineEnding
  deriving (Eq, Ord, Show)

data LineEnding
  = EOF
  | CR
  | LF
  | CRLF
  deriving (Bounded, Enum, Eq, Ord, Show)

linesFromString :: String -> Lines
linesFromString = Lines . go
  where
  go = \case
    "" -> [Line "" EOF]
    s  -> let (line, rest) = takeLine s in line : either (const []) go rest
{-# INLINE linesFromString #-}

takeLine :: String -> (Line, Either String String)
takeLine = go id where
  go line = \case
    ""             -> (mk EOF, Left "")
    '\r':'\n':rest -> (mk CRLF, Right rest)
    '\r':rest      -> (mk CR, Right rest)
    '\n':rest      -> (mk LF, Right rest)
    c   :rest      -> go (line . (c:)) rest
    where
    mk = Line (line "")
{-# INLINE takeLine #-}

(!) :: Lines -> Span.Pos -> Line
Lines lines ! pos = lines !! Span.line pos
{-# INLINE (!) #-}

infixl 9 !


line :: (Has Parser sig m, Has (Reader Lines) sig m) => m Line
line = do
  pos <- position
  asks (! pos)
{-# INLINE line #-}
