{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- |
Module      : Data.PEM.Types
License     : BSD-style
Copyright   : (c) 2010-2018 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : portable
-}

module Data.PEM.Types
  ( PEM (..)
  ) where

import           Control.DeepSeq ( NFData (..) )
import           Data.ByteString ( ByteString )
import           GHC.Generics ( Generic )

-- | A type representing single PEM sections.
data PEM = PEM
  { pemName    :: String
    -- ^ The name of the section, found after the dash BEGIN tag.
  , pemHeader  :: [(String, ByteString)]
    -- ^ Optional key-value pairs header. The library does not currently
    -- serialize headers.
  , pemContent :: ByteString
    -- ^ Binary content of the section.
  }
  deriving (Eq, Generic, NFData, Show)
