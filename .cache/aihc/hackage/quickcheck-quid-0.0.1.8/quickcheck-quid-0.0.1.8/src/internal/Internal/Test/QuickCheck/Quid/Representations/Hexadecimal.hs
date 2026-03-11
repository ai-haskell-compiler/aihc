{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Internal.Test.QuickCheck.Quid.Representations.Hexadecimal
    ( Hexadecimal (..)
    )
    where

import Control.DeepSeq
    ( NFData )
import Data.Bifunctor
    ( first )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import GHC.Generics
    ( Generic )
import Internal.Test.QuickCheck.Quid
    ( Quid (..) )
import Internal.Test.QuickCheck.Quid.Combinators.Prefix
    ( Prefix (..) )
import Numeric
    ( readHex, showHex )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Function )

--------------------------------------------------------------------------------
-- Hexadecimal representation
--------------------------------------------------------------------------------

newtype Hexadecimal a = Hexadecimal { unHexadecimal :: a }
    deriving stock (Data, Eq, Generic, Ord)
    deriving newtype (Hashable, NFData, Num)
    deriving anyclass Function

deriving via Prefix "0x" (AsHex Natural) instance Read (Hexadecimal Quid)
deriving via Prefix "0x" (AsHex Natural) instance Show (Hexadecimal Quid)

newtype AsHex a = AsHex a

instance (Eq a, Num a) => Read (AsHex a) where
    readsPrec _ = fmap (first AsHex) <$> readHex

instance (Integral a, Show a) => Show (AsHex a) where
    show (AsHex n) = showHex n ""
