{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Bits.Types.Builtin where

import Control.DeepSeq
import Data.Vector.Storable
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise

-- | Type wrapper to prefer builting operations.
newtype Builtin a = Builtin a deriving (BitWise, Eq, Show, Storable, Generic, NFData)
