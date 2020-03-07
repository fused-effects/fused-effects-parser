module Control.Effect.Parser.Path
( Path(..)
, path
) where

import Control.Effect.Reader

newtype Path = Path { getPath :: FilePath }
  deriving (Eq, Ord, Show)

path :: Has (Reader Path) sig m => m FilePath
path = asks getPath
{-# INLINE path #-}
