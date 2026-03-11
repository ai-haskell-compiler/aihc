{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Bits.Types.Broadword where

import Control.DeepSeq
import Data.Vector.Storable
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise

-- | Type wrapper to prefer broadword operations.
newtype Broadword a = Broadword a deriving (BitWise, Eq, Show, Storable, Generic, NFData)
