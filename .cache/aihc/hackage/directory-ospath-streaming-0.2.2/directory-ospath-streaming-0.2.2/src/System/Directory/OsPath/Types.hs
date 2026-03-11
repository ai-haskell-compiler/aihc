-- |
-- Module:     System.Directory.OsPath.Types
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Directory.OsPath.Types
  ( SymlinkType(..)
  , FileType(..)
  , Basename(..)
  , Relative(..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic, Generic1)

data SymlinkType = Regular | Symlink
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData SymlinkType

data FileType
  = File {-# UNPACK #-} !SymlinkType
  | Directory {-# UNPACK #-} !SymlinkType
  | Other {-# UNPACK #-} !SymlinkType
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData FileType

-- | Basename part of filename, without directory separators.
newtype Basename a = Basename { unBasename :: a }
  deriving (Eq, Ord, Show, Generic, Generic1, NFData, Functor, Foldable, Traversable)

-- | Filename relative to some other path.
newtype Relative a = Relative { unRelative :: a }
  deriving (Eq, Ord, Show, Generic, Generic1, NFData, Functor, Foldable, Traversable)
