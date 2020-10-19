{-# LANGUAGE LambdaCase #-}
module Control.Effect.Parser.Source
( Source(..)
, Line(..)
, LineEnding(..)
, sourceFromString
, readSourceFromFile
, (!)
, (!..)
) where

import qualified Control.Effect.Parser.Span as Span
import           Control.Exception (assert)
import qualified Data.List.NonEmpty as NE
import qualified Prettyprinter as P

data Source = Source
  { path  :: Maybe FilePath
  , lines :: NE.NonEmpty Line
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
  go s = let (line, rest) = takeLine s in maybe (NE.fromList [ line ]) (NE.cons line . go) rest
{-# INLINE sourceFromString #-}

readSourceFromFile :: FilePath -> IO Source
readSourceFromFile path = sourceFromString (Just path) <$> readFile path
{-# INLINE readSourceFromFile #-}


takeLine :: String -> (Line, Maybe String)
takeLine = go id where
  go line = \case
    ""             -> (Line (line "") EOF,  Nothing)
    '\r':'\n':rest -> (Line (line "") CRLF, Just rest)
    '\r':     rest -> (Line (line "") CR,   Just rest)
    '\n':     rest -> (Line (line "") LF,   Just rest)
    c   :     rest -> go (line . (c:)) rest
{-# INLINE takeLine #-}

(!) :: Source -> Span.Pos -> Line
src ! pos = NE.head $ src !.. Span.Span pos pos
{-# INLINE (!) #-}

infixl 9 !

(!..) :: Source -> Span.Span -> NE.NonEmpty Line
Source _ lines !.. span
  = assert (endLine >= startLine)
  $ NE.fromList
  $ take (endLine - startLine + 1)
  $ NE.drop startLine lines
  where
  startLine = Span.line (Span.start span)
  endLine   = Span.line (Span.end   span)
{-# INLINE (!..) #-}

infixl 9 !..
