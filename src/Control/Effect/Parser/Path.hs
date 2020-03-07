{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Effect.Parser.Path
( Path(..)
, path
) where

import Control.Effect.Reader
import Data.Text.Prettyprint.Doc

newtype Path = Path { getPath :: FilePath }
  deriving (Eq, Ord, Pretty, Show)

path :: Has (Reader Path) sig m => m FilePath
path = asks getPath
{-# INLINE path #-}
