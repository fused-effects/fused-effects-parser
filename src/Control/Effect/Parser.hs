{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Parser
( -- * Re-exports
  Algebra
, Has
, run
) where

import Control.Algebra
import Source.Span

data Parser m k
  = forall a . Accept (Char -> Maybe a) (a -> m k)
  | forall a . Label (m a) String (a -> m k)
  | Unexpected String
  | Position (Pos -> m k)
