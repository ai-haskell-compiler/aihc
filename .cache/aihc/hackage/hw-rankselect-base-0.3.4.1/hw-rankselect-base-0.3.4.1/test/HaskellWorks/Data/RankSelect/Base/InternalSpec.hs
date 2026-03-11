module HaskellWorks.Data.RankSelect.Base.InternalSpec
  ( spec
  ) where

import Control.Monad                              (mfilter)
import Data.Bits                                  (popCount)
import HaskellWorks.Data.RankSelect.Base.Internal
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.Base.InternalSpec" $ do
  describe "Bmi2 and broadword implementations match" $ do
    it "64-bit" $ requireProperty $ do
      s <- forAll $ mfilter (/= 0) (G.word64 R.constantBounded)
      r <- forAll $ G.word64 (R.linear 0 (fromIntegral (popCount s)))
      select1Word64Broadword s r === select1Word64Bmi2 s r
    it "32-bit" $ requireProperty $ do
      s <- forAll $ mfilter (/= 0) (G.word32 R.constantBounded)
      r <- forAll $ G.word64 (R.linear 0 (fromIntegral (popCount s)))
      select1Word32Broadword s r === select1Word32Bmi2 s r
