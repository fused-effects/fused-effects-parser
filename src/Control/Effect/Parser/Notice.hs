{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Notice
( Level(..)
, prettyLevel
, Notice(..)
, level_
, excerpt_
, reason_
, context_
, prettyNotice
) where

import           Control.Effect.Parser.Excerpt
import           Control.Effect.Parser.Lens
import           Control.Effect.Parser.Source
import           Control.Effect.Parser.Span as Span
import           Data.Maybe (fromMaybe)
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle, Color(..), color)
import qualified Prettyprinter.Render.Terminal as ANSI

data Level
  = Warn
  | Error
  deriving (Eq, Ord, Show)

prettyLevel :: Level -> Doc AnsiStyle
prettyLevel = \case
  Warn  -> magenta (pretty "warning")
  Error -> red     (pretty "error")


data Notice = Notice
  { level    :: !(Maybe Level)
  , excerpt  :: {-# UNPACK #-} !Excerpt
  , reason   :: !(Doc AnsiStyle)
  , context  :: ![Doc AnsiStyle]
  }
  deriving (Show)

level_ :: Lens' Notice (Maybe Level)
level_ = lens level $ \ n level -> n{ level }

excerpt_ :: Lens' Notice Excerpt
excerpt_ = lens excerpt $ \ n excerpt -> n{ excerpt }

reason_ :: Lens' Notice (Doc AnsiStyle)
reason_ = lens reason $ \ n reason -> n{ reason }

context_ :: Lens' Notice [Doc AnsiStyle]
context_ = lens context $ \ n context -> n{ context }

prettyNotice :: Notice -> Doc AnsiStyle
prettyNotice (Notice level (Excerpt path line span) reason context) = vsep
  ( nest 2 (group (fillSep
    [ bold (pretty (fromMaybe "(interactive)" path)) <> colon <> pos (Span.start span) <> colon <> foldMap ((space <>) . (<> colon) . prettyLevel) level
    , reason
    ]))
  : blue (pretty (succ (Span.line (Span.start span)))) <+> align (vcat
    [ blue (pretty '|') <+> prettyLine line
    , blue (pretty '|') <+> padding span <> caret span
    ])
  : context)
  where
  pos (Pos l c) = bold (pretty (succ l)) <> colon <> bold (pretty (succ c))

  padding (Span (Pos _ c) _) = pretty (replicate c ' ')

  caret (Span start@(Pos sl sc) end@(Pos el ec))
    | start == end = green (pretty '^')
    | sl    == el  = green (pretty (replicate (ec - sc) '~'))
    | otherwise    = green (pretty "^…")

  bold = annotate ANSI.bold

prettyLine :: Line -> Doc AnsiStyle
prettyLine (Line line end) = pretty line <> blue (pretty end)


red, green, blue, magenta :: Doc AnsiStyle -> Doc AnsiStyle
red     = annotate $ color Red
green   = annotate $ color Green
blue    = annotate $ color Blue
magenta = annotate $ color Magenta
