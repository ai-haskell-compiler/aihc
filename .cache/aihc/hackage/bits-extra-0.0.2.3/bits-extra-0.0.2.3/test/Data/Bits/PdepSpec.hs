
module Data.Bits.PdepSpec (spec) where

import Data.Bits.Pdep
import Data.Bits.Pdep.Slow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "Data.Bits.PdepSpec" $ do
  describe "Emulated pdep and native pdep give the same answer" $ do
    it "Word64" $ require $ property $ do
      a <- forAll $ G.word64 R.constantBounded
      b <- forAll $ G.word64 R.constantBounded
      pdep a b === slowPdep a b
    it "Word32" $ require $ property $ do
      a <- forAll $ G.word32 R.constantBounded
      b <- forAll $ G.word32 R.constantBounded
      pdep a b === slowPdep a b
    it "Word16" $ require $ property $ do
      a <- forAll $ G.word16 R.constantBounded
      b <- forAll $ G.word16 R.constantBounded
      pdep a b === slowPdep a b
    it "Word8" $ require $ property $ do
      a <- forAll $ G.word8 R.constantBounded
      b <- forAll $ G.word8 R.constantBounded
      pdep a b === slowPdep a b
    it "Word" $ require $ property $ do
      a <- forAll $ G.word R.constantBounded
      b <- forAll $ G.word R.constantBounded
      pdep a b === slowPdep a b
