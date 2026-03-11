{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Tests.EMD (
    emdTests
  ) where

import           Control.Monad
import           Data.Functor.Identity
import           Data.Proxy
import           GHC.TypeNats
import           Hedgehog
import           Numeric.EMD
import           Numeric.EMD.Sift
import           Test.Tasty
import           Tests.Util
import qualified Data.Vector           as UV
import qualified Hedgehog.Range        as Range

emdTests :: TestTree
emdTests = groupTree $$(discover)

prop_iemd_default :: Property
prop_iemd_default = iemdProp defaultEO

prop_orthog_default :: Property
prop_orthog_default = orthogProp defaultEO

edtEO :: KnownNat n => EMDOpts UV.Vector n Double
edtEO = (defaultEO @UV.Vector)
    { eoSifter = siftEnergyDiff 0.01 0.01
        `siftOr` siftTimes 100
    }

prop_iemd_edt :: Property
prop_iemd_edt = iemdProp edtEO

prop_orthog_edt :: Property
prop_orthog_edt = orthogProp edtEO

sCondEO :: KnownNat n => EMDOpts UV.Vector (n + 1) Double
sCondEO = (defaultEO @UV.Vector)
    { eoSifter = siftSCond 10
        `siftOr` siftTimes 100
    }

prop_iemd_sCond :: Property
prop_iemd_sCond = iemdProp sCondEO

prop_orthog_sCond :: Property
prop_orthog_sCond = orthogProp sCondEO


iemdProp :: (forall n. KnownNat n => EMDOpts UV.Vector (n + 1) Double) -> Property
iemdProp eo = property $ withSize (Range.linear 1 8) $ \(_ :: Proxy n) -> do
    xs <- forAll $ generateData @n
    tripping (CE xs) (emd @_ @_ @(2^n-1) eo . getCE) (Identity . CE . iemd)

orthogProp :: (forall n. KnownNat n => EMDOpts UV.Vector (n + 1) Double) -> Property
orthogProp eo = property $ withSize (Range.linear 8 10) $ \(_ :: Proxy n) -> do
    xs   <- forAll $ generateData @n
    let imfs = emdIMFs (emd @_ @_ @(2^n-1) eo xs)
        orthoMatrix =
          [ ((i, j), (x, y), dot x y / sqrt (dot x x * dot y y))
          | (i, x) <- zip indices imfs
          , (j, y) <- zip indices imfs
          , i < j
          ]
        badOrthos = filter (\(_,_,d) -> abs d > 0.5) orthoMatrix
        fracBad :: Double
        fracBad = fromIntegral (length badOrthos)
                / fromIntegral (length orthoMatrix)
    annotateShow orthoMatrix
    annotateShow fracBad
    when (length orthoMatrix < 6) discard
    assert $ fracBad <= 0.5
  where
    indices :: [Int]
    indices = [1..]
