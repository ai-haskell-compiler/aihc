{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Internal.Test.QuickCheck.Quid.Representations.Decimal
    ( Decimal (..)
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
import Numeric
    ( showInt )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Function )
import Text.ParserCombinators.ReadP
    ( readP_to_S, skipSpaces )
import Text.Read.Lex
    ( readDecP )

--------------------------------------------------------------------------------
-- Decimal representation
--------------------------------------------------------------------------------

newtype Decimal a = Decimal { unDecimal :: a }
    deriving stock (Data, Eq, Generic, Ord)
    deriving newtype (Hashable, NFData, Num)
    deriving anyclass Function

deriving via AsDecimal Natural instance Read (Decimal Quid)
deriving via AsDecimal Natural instance Show (Decimal Quid)

newtype AsDecimal a = AsDecimal a

instance (Eq a, Num a) => Read (AsDecimal a) where
    readsPrec _ = fmap (first AsDecimal) <$> readP_to_S (skipSpaces >> readDecP)

instance (Integral a, Show a) => Show (AsDecimal a) where
    show (AsDecimal n) = showInt n ""
