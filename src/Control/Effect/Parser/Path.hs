{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Effect.Parser.Path
( Path(..)
, path
) where

import Control.Effect.Reader
import Prettyprinter

newtype Path = Path { getPath :: FilePath }
  deriving (Eq, Ord, Pretty, Show)

path :: Has (Reader Path) sig m => m Path
path = ask
{-# INLINE path #-}
