{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Internal.Test.QuickCheck.Quid
    where

import Control.DeepSeq
    ( NFData )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Function (..)
    , Gen
    , chooseInteger
    , coarbitraryShow
    , functionMap
    , shrinkMapBy
    , sized
    )
import Test.QuickCheck.Function
    ( (:->) )

import qualified Data.List as L

--------------------------------------------------------------------------------
-- Quids
--------------------------------------------------------------------------------

newtype Quid = Quid
    { unQuid :: Natural }
    deriving (Data, Eq, Generic, Ord)
    deriving newtype (Hashable, NFData, Num)

instance Arbitrary Quid where
    arbitrary = arbitraryQuid
    shrink = shrinkQuid

instance CoArbitrary Quid where
    coarbitrary = coarbitraryQuid

instance Function Quid where
    function = functionQuid

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary quids
--------------------------------------------------------------------------------

arbitraryQuid :: Gen Quid
arbitraryQuid = sized $ \i -> chooseQuid (Quid 0, Quid $ (2 ^ max 0 i) - 1)

chooseQuid :: (Quid, Quid) -> Gen Quid
chooseQuid (Quid n1, Quid n2) = Quid <$> chooseNatural (n1, n2)

coarbitraryQuid :: Quid -> Gen a -> Gen a
coarbitraryQuid = coarbitraryShow . unQuid

functionQuid :: (Quid -> a) -> Quid :-> a
functionQuid = functionMap (show . unQuid) (Quid . read)

shrinkQuid :: Quid -> [Quid]
shrinkQuid = shrinkMapBy Quid unQuid shrinkNatural

--------------------------------------------------------------------------------
-- Conversion between quids and natural numbers
--------------------------------------------------------------------------------

naturalToQuid :: Natural -> Quid
naturalToQuid = Quid

quidToNatural :: Quid -> Natural
quidToNatural = unQuid

--------------------------------------------------------------------------------
-- Natural number support
--------------------------------------------------------------------------------

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (p, q) = fromIntegral @Integer @Natural <$>
    chooseInteger (fromIntegral p, fromIntegral q)

shrinkNatural :: Natural -> [Natural]
shrinkNatural n
    | n == 0 = []
    | otherwise = L.nub $ 0 : as <> bs
  where
    as = takeWhile (<= n `div` 2) (iterate (* 2) 1)
    bs = (n -) <$> reverse as
