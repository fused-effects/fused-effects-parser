{-# LANGUAGE LambdaCase #-}
module Control.Effect.Parser.Source
( Source(..)
, Line(..)
, LineEnding(..)
, sourceFromString
, (!)
) where

import qualified Control.Effect.Parser.Span as Span
import qualified Prettyprinter as P

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
    ""             -> (Line (line "") EOF,  Left "")
    '\r':'\n':rest -> (Line (line "") CRLF, Right rest)
    '\r':     rest -> (Line (line "") CR,   Right rest)
    '\n':     rest -> (Line (line "") LF,   Right rest)
    c   :     rest -> go (line . (c:)) rest
{-# INLINE takeLine #-}

(!) :: Source -> Span.Pos -> Line
Source _ lines ! pos = lines !! Span.line pos
{-# INLINE (!) #-}

infixl 9 !
