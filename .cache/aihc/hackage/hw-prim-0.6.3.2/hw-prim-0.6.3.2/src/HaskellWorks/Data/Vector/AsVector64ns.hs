{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Vector.AsVector64ns
  ( AsVector64ns(..)
  ) where

import Data.Word
import HaskellWorks.Data.Vector.AsVector8ns (asVector8ns)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as DVS

class AsVector64ns a where
  -- | Represent the value as a list of Vector of 'n' Word64 chunks.  The last chunk will
  -- also be of the specified chunk size filled with trailing zeros.
  asVector64ns :: Int -> a -> [DVS.Vector Word64]

instance AsVector64ns LBS.ByteString where
  asVector64ns n = asVector64ns n . LBS.toChunks
  {-# INLINE asVector64ns #-}

instance AsVector64ns [BS.ByteString] where
  asVector64ns n bss = DVS.unsafeCast <$> asVector8ns (n * 8) bss
  {-# INLINE asVector64ns #-}
