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
  { level   :: Maybe Level
  , excerpt :: {-# UNPACK #-} !Excerpt
  , reason  :: Doc AnsiStyle
  , context :: [Doc AnsiStyle]
  }
  deriving (Show)

instance Eq Notice where
  Notice l1 e1 r1 c1 == Notice l2 e2 r2 c2
    =  l1 == l2
    && e1 == e2
    && show r1 == show r2
    && map show c1 == map show c2

prettyNotice :: Notice -> Doc AnsiStyle
prettyNotice (Notice level (Excerpt path line span) reason context) = vsep
  ( nest 2 (group (vsep [bold (pretty path) <> colon <> bold (pretty (succ (Span.line (Span.start span)))) <> colon <> bold (pretty (succ (Span.column (Span.start span)))) <> colon <> maybe mempty ((space <>) . (<> colon) . prettyLevel) level, reason]))
  : blue (pretty (succ (Span.line (Span.start span)))) <+> align (fold
    [ blue (pretty '|') <+> pretty line <> if "\n" `isSuffixOf` line then mempty else blue (pretty "<EOF>") <> hardline
    , blue (pretty '|') <+> caret span
    ])
  : context) where
  caret span = pretty (replicate (Span.column (Span.start span)) ' ') <> prettySpan span

  prettySpan (Span start end)
    | start == end                     = green (pretty '^')
    | Span.line start == Span.line end = green (pretty (replicate (Span.column end - Span.column start) '~'))
    | otherwise                        = green (pretty "^…")

  bold = annotate ANSI.bold


red, green, blue, magenta :: Doc AnsiStyle -> Doc AnsiStyle
red     = annotate $ color Red
green   = annotate $ color Green
blue    = annotate $ color Blue
magenta = annotate $ color Magenta
