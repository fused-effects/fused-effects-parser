{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Notice
( Level(..)
, Notice(..)
, level_
, excerpt_
, reason_
, context_
, reAnnotateNotice
, Style(..)
, ansiStyle
, prettyNoticeWith
, prettyNotice
) where

import Control.Effect.Parser.Excerpt
import Control.Effect.Parser.Lens
import Control.Effect.Parser.Source
import Control.Effect.Parser.Span as Span
import Data.Maybe (fromMaybe)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..), bold, color)

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

reAnnotateNotice :: (a -> b) -> (Notice a -> Notice b)
reAnnotateNotice f Notice{ level, excerpt, reason, context} = Notice{ level, excerpt, reason = reAnnotate f reason, context = map (reAnnotate f) context }


data Style a = Style
  { pathStyle   :: Doc a -> Doc a
  , levelStyle  :: Level -> Doc a -> Doc a
  , posStyle    :: Doc a -> Doc a
  , gutterStyle :: Doc a -> Doc a
  , eofStyle    :: Doc a -> Doc a
  , caretStyle  :: Doc a -> Doc a
  }

ansiStyle :: Style AnsiStyle
ansiStyle = Style
  { pathStyle   = annotate bold
  , levelStyle  = \case
    Warn  -> annotate (color Magenta)
    Error -> annotate (color Red)
  , posStyle    = annotate bold
  , gutterStyle = annotate (color Blue)
  , eofStyle    = annotate (color Blue)
  , caretStyle  = annotate (color Green)
  }

prettyNoticeWith :: Style a -> Notice a -> Doc a
prettyNoticeWith Style{ pathStyle, levelStyle, posStyle, gutterStyle, eofStyle, caretStyle } (Notice level (Excerpt path line span) reason context) = vsep
  ( nest 2 (group (fillSep
    [ pathStyle (pretty (fromMaybe "(interactive)" path)) <> colon <> pos (Span.start span) <> colon <> foldMap ((space <>) . (<> colon) . (levelStyle <*> pretty)) level
    , reason
    ]))
  : gutterStyle (pretty (succ (Span.line (Span.start span)))) <+> align (vcat
    [ gutterStyle (pretty '|') <+> prettyLine line
    , gutterStyle (pretty '|') <+> padding span <> caret span
    ])
  : context)
  where
  pos (Pos l c) = posStyle (pretty (succ l)) <> colon <> posStyle (pretty (succ c))

  padding (Span (Pos _ c) _) = pretty (replicate c ' ')

  caret (Span start@(Pos sl sc) end@(Pos el ec))
    | start == end = caretStyle (pretty '^')
    | sl    == el  = caretStyle (pretty (replicate (ec - sc) '~'))
    | otherwise    = caretStyle (pretty "^…")

  prettyLine (Line line end) = pretty line <> eofStyle (pretty end)


prettyNotice :: Notice AnsiStyle -> Doc AnsiStyle
prettyNotice = prettyNoticeWith ansiStyle
