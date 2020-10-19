{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Source
( Source(..)
, span_
, contents_
, Line(..)
, LineEnding(..)
, sourceFromString
, readSourceFromFile
, (!)
, (!..)
) where

import           Control.Effect.Parser.Lens
import qualified Control.Effect.Parser.Span as Span
import           Control.Exception (assert)
import qualified Data.List.NonEmpty as NE
import           Prelude hiding (span)
import qualified Prettyprinter as P

data Source = Source
  { path     :: Maybe FilePath
  , span     :: Span.Span
  , contents :: String -- FIXME: Text
  , lines    :: NE.NonEmpty Line
  }
  deriving (Eq, Ord, Show)

-- | A lens over the 'Span.Span' from a 'Source'.
--
-- Note that it is the caller’s responsibility to ensure that this span and the 'lines' are in agreement as to line numbers.
span_ :: Lens' Source Span.Span
span_ = lens span $ \ e span -> e{ span }
{-# INLINE span_ #-}

contents_ :: Lens' Source String
contents_ = lens contents $ \ e contents -> e{ contents }
{-# INLINE contents_ #-}


data Line = Line Int String LineEnding -- FIXME: use (byte? character?) ranges instead of copying the contents?
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


sourceFromString :: Maybe FilePath -> Int -> String -> Source
sourceFromString path line contents = Source path span contents lines
  where
  span = Span.Span (Span.Pos line 0) (let Line i s e = NE.last lines in Span.Pos i (length s + case e of
    EOF  -> 0
    CRLF -> 2
    _    -> 1))
  lines = go line contents
  go i s = let (line, rest) = takeLine i s in maybe (NE.fromList [ line ]) (NE.cons line . go (succ i)) rest
{-# INLINE sourceFromString #-}

readSourceFromFile :: FilePath -> IO Source
readSourceFromFile path = sourceFromString (Just path) 0 <$> readFile path
{-# INLINE readSourceFromFile #-}


takeLine :: Int -> String -> (Line, Maybe String)
takeLine i = go id where
  go line = \case
    ""             -> (Line i (line "") EOF,  Nothing)
    '\r':'\n':rest -> (Line i (line "") CRLF, Just rest)
    '\r':     rest -> (Line i (line "") CR,   Just rest)
    '\n':     rest -> (Line i (line "") LF,   Just rest)
    c   :     rest -> go (line . (c:)) rest
{-# INLINE takeLine #-}


(!) :: Source -> Span.Pos -> Line
src ! pos = NE.head $ src !.. Span.Span pos pos
{-# INLINE (!) #-}

infixl 9 !

(!..) :: Source -> Span.Span -> NE.NonEmpty Line
Source _ _ _ lines !.. span
  = assert (endLine >= startLine)
  $ NE.fromList
  $ takeWhile (\ (Line i _ _) -> i <= endLine)
  $ NE.dropWhile (\ (Line i _ _) -> i < startLine) lines
  where
  startLine = Span.line (Span.start span)
  endLine   = Span.line (Span.end   span)
{-# INLINE (!..) #-}

infixl 9 !..
