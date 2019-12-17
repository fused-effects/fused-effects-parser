{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Control.Carrier.Parser.Church
( -- * Parser carrier
  ParserC(..)
) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Source.Span

newtype ParserC m a = ParserC
  { runParserC
    :: forall r
    .  (Pos -> String -> a -> m r)           -- success
    -> (Pos -> Maybe (Doc AnsiStyle) -> m r) -- empty
    -> (Pos -> Maybe (Doc AnsiStyle) -> m r) -- cut
    -> Pos
    -> String
    -> m r
  }
  deriving (Functor)
