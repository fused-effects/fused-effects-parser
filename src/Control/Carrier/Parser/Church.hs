{-# LANGUAGE DeriveTraversable, RankNTypes #-}
module Control.Carrier.Parser.Church
( -- * Parser carrier
  ParserC(..)
  -- * Parser effect
, module Control.Effect.Parser
) where

import Control.Effect.Parser
import Control.Monad (ap)
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

instance Applicative (ParserC m) where
  pure a = ParserC (\ just _ _ pos input -> just pos input a)
  (<*>) = ap

instance Monad (ParserC m) where
  m >>= f = ParserC (\ just nothing fail -> runParserC m (\ pos input a -> runParserC (f a) just nothing fail pos input) nothing fail)


data Result a = Result
  { resultPos   :: {-# UNPACK #-} !Pos
  , resultState :: Either (Maybe (Doc AnsiStyle)) (String, a)
  }
  deriving (Foldable, Functor, Show, Traversable)
