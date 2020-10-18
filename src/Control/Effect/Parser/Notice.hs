{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Notice
( Level(..)
, Notice(..)
, level_
, excerpt_
, reason_
, context_
, Style(..)
, ansiStyle
, prettyNotice
) where

import           Control.Effect.Parser.Excerpt
import           Control.Effect.Parser.Lens
import           Control.Effect.Parser.Source
import           Control.Effect.Parser.Span as Span
import           Data.Maybe (fromMaybe)
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle, Color(..), bold, color)
import qualified Prettyprinter.Render.Terminal as ANSI

data Level
  = Warn
  | Error
  deriving (Eq, Ord, Show)

instance Pretty Level where
  pretty = \case
    Warn  -> pretty "warning"
    Error -> pretty "error"


data Notice a = Notice
  { level   :: !(Maybe Level)
  , excerpt :: {-# UNPACK #-} !Excerpt
  , reason  :: !(Doc a)
  , context :: ![Doc a]
  }
  deriving (Show)

level_ :: Lens' (Notice a) (Maybe Level)
level_ = lens level $ \ n level -> n{ level }

excerpt_ :: Lens' (Notice a) Excerpt
excerpt_ = lens excerpt $ \ n excerpt -> n{ excerpt }

reason_ :: Lens' (Notice a) (Doc a)
reason_ = lens reason $ \ n reason -> n{ reason }

context_ :: Lens' (Notice a) [Doc a]
context_ = lens context $ \ n context -> n{ context }


data Style a = Style
  { pathStyle   :: a
  , levelStyle  :: Level -> a
  , posStyle    :: a
  , gutterStyle :: a
  , eofStyle    :: a
  , caretStyle  :: a
  }

ansiStyle :: Style AnsiStyle
ansiStyle = Style
  { pathStyle   = bold
  , levelStyle  = \case
    Warn  -> color Magenta
    Error -> color Red
  , posStyle    = bold
  , gutterStyle = color Blue
  , eofStyle    = color Blue
  , caretStyle  = color Green
  }

prettyNotice :: Notice AnsiStyle -> Doc AnsiStyle
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

  prettyLevel = \case
    Warn  -> magenta (pretty "warning")
    Error -> red     (pretty "error")

  prettyLine (Line line end) = pretty line <> blue (pretty end)


red, green, blue, magenta :: Doc AnsiStyle -> Doc AnsiStyle
red     = annotate $ color Red
green   = annotate $ color Green
blue    = annotate $ color Blue
magenta = annotate $ color Magenta
