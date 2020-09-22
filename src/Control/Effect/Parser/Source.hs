{-# LANGUAGE LambdaCase #-}
module Control.Effect.Parser.Source
( Source(..)
, Line(..)
, LineEnding(..)
, sourceFromString
, (!)
, line
, source
) where

import           Control.Effect.Parser
import           Control.Effect.Reader
import qualified Prettyprinter as P
import qualified Source.Span as Span

data Source = Source
  { path  :: Maybe FilePath
  , lines :: [Line]
  }
  deriving (Eq, Ord, Show)

data Line = Line String LineEnding
  deriving (Eq, Ord, Show)

data LineEnding
  = EOF
  | CR
  | LF
  | CRLF
  deriving (Bounded, Enum, Eq, Ord, Show)

instance P.Pretty LineEnding where
  pretty = P.pretty . \case
    EOF  -> "<end of input>"
    CRLF -> "\\r\\n"
    CR   -> "\\r"
    LF   -> "\\n"


sourceFromString :: Maybe FilePath -> String -> Source
sourceFromString path = Source path . go
  where
  go s = let (line, rest) = takeLine s in line : either (const []) go rest
{-# INLINE sourceFromString #-}

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

(!) :: Source -> Span.Pos -> Line
Source _ lines ! pos = lines !! Span.line pos
{-# INLINE (!) #-}

infixl 9 !


line :: (Has Parser sig m, Has (Reader Source) sig m) => m Line
line = do
  pos <- position
  asks (! pos)
{-# INLINE line #-}

source :: Has (Reader Source) sig m => m Source
source = ask
{-# INLINE source #-}
