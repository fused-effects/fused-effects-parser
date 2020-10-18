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

prettyNoticeWith :: Style a -> Notice a -> Doc a
prettyNoticeWith Style{ pathStyle, levelStyle, posStyle, gutterStyle, eofStyle, caretStyle } (Notice level (Excerpt path line span) reason context) = vsep
  ( nest 2 (group (fillSep
    [ annotate pathStyle (pretty (fromMaybe "(interactive)" path)) <> colon <> pos (Span.start span) <> colon <> foldMap ((space <>) . (<> colon) . (annotate . levelStyle <*> pretty)) level
    , reason
    ]))
  : annotate gutterStyle (pretty (succ (Span.line (Span.start span)))) <+> align (vcat
    [ annotate gutterStyle (pretty '|') <+> prettyLineWith line
    , annotate gutterStyle (pretty '|') <+> padding span <> caret span
    ])
  : context)
  where
  pos (Pos l c) = annotate posStyle (pretty (succ l)) <> colon <> annotate posStyle (pretty (succ c))

  padding (Span (Pos _ c) _) = pretty (replicate c ' ')

  caret (Span start@(Pos sl sc) end@(Pos el ec))
    | start == end = annotate caretStyle (pretty '^')
    | sl    == el  = annotate caretStyle (pretty (replicate (ec - sc) '~'))
    | otherwise    = annotate caretStyle (pretty "^…")

  prettyLineWith (Line line end) = pretty line <> annotate eofStyle (pretty end)


prettyNotice :: Notice AnsiStyle -> Doc AnsiStyle
prettyNotice = prettyNoticeWith ansiStyle
