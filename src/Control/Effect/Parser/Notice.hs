{-# LANGUAGE LambdaCase #-}
module Control.Effect.Parser.Notice
( Level(..)
, prettyLevel
, Notice(..)
, prettyNotice
) where

import           Control.Effect.Parser.Excerpt
import           Data.Foldable (fold)
import           Data.List (isSuffixOf)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color(..), color)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           Source.Span as Span

data Level
  = Warn
  | Error
  deriving (Eq, Ord, Show)

prettyLevel :: Level -> Doc AnsiStyle
prettyLevel = \case
  Warn  -> magenta (pretty "warning")
  Error -> red     (pretty "error")


data Notice = Notice
  { level    :: Maybe Level
  , excerpt  :: {-# UNPACK #-} !Excerpt
  , reason   :: Maybe (Doc AnsiStyle)
  , expected :: Set.Set String
  , context  :: [Doc AnsiStyle]
  }
  deriving (Show)

prettyNotice :: Notice -> Doc AnsiStyle
prettyNotice (Notice level (Excerpt path line span) reason expected context) = vsep
  ( nest 2 (group (fillSep
    ( bold (pretty path) <> colon <> pos (Span.start span) <> colon <> maybe mempty ((space <>) . (<> colon) . prettyLevel) level
    : fromMaybe (fillSep (map pretty (words "unknown error"))) reason <> (if null expected then mempty else comma)
    : if null expected then [] else pretty "expected" <> colon : punctuate comma (map pretty (Set.toList expected)))))
  : blue (pretty (succ (Span.line (Span.start span)))) <+> align (fold
    [ blue (pretty '|') <+> pretty line <> if "\n" `isSuffixOf` line then mempty else blue (pretty "<end of input>") <> hardline
    , blue (pretty '|') <+> padding span <> caret span
    ])
  : context)
  where
  pos (Pos l c) = bold (pretty (succ l)) <> colon <> bold (pretty (succ c))

  padding (Span (Pos _ c) _) = pretty (replicate c ' ')

  caret (Span start end)
    | start == end                     = green (pretty '^')
    | Span.line start == Span.line end = green (pretty (replicate (Span.column end - Span.column start) '~'))
    | otherwise                        = green (pretty "^…")

  bold = annotate ANSI.bold


red, green, blue, magenta :: Doc AnsiStyle -> Doc AnsiStyle
red     = annotate $ color Red
green   = annotate $ color Green
blue    = annotate $ color Blue
magenta = annotate $ color Magenta
