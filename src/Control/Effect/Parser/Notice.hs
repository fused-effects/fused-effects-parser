{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Effect.Parser.Notice
( Level(..)
, Notice(..)
, level_
, source_
, reason_
, context_
, reAnnotateNotice
, Style(..)
, identityStyle
, prettyNoticeWith
, prettyNotice
) where

import Control.Effect.Parser.Lens
import Control.Effect.Parser.Source
import Control.Effect.Parser.Span as Span
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Prettyprinter

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
  , source  :: {-# UNPACK #-} !Source
  , reason  :: !(Doc a)
  , context :: ![Doc a]
  }
  deriving (Show)

level_ :: Lens' (Notice a) (Maybe Level)
level_ = lens level $ \ n level -> n{ level }

source_ :: Lens' (Notice a) Source
source_ = lens source $ \ n source -> n{ source }

reason_ :: Lens' (Notice a) (Doc a)
reason_ = lens reason $ \ n reason -> n{ reason }

context_ :: Lens' (Notice a) [Doc a]
context_ = lens context $ \ n context -> n{ context }

reAnnotateNotice :: (a -> b) -> (Notice a -> Notice b)
reAnnotateNotice f Notice{ level, source, reason, context} = Notice{ level, source, reason = reAnnotate f reason, context = map (reAnnotate f) context }


data Style a = Style
  { pathStyle   :: Doc a -> Doc a
  , levelStyle  :: Level -> Doc a -> Doc a
  , posStyle    :: Doc a -> Doc a
  , gutterStyle :: Doc a -> Doc a
  , eofStyle    :: Doc a -> Doc a
  , caretStyle  :: Doc a -> Doc a
  }

identityStyle :: Style a
identityStyle = Style
  { pathStyle   = id
  , levelStyle  = const id
  , posStyle    = id
  , gutterStyle = id
  , eofStyle    = id
  , caretStyle  = id
  }

prettyNoticeWith :: Style a -> Notice a -> Doc a
prettyNoticeWith Style{ pathStyle, levelStyle, posStyle, gutterStyle, eofStyle, caretStyle } (Notice level (Source path span _ (line:|_)) reason context) = concatWith (surround hardline)
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

  prettyLine (Line _ line end) = pretty line <> eofStyle (pretty end)


prettyNotice :: Notice a -> Doc a
prettyNotice = prettyNoticeWith identityStyle
