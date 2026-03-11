module Data.Bits.PextSpec (spec) where

import Data.Bits.Pext
import Data.Bits.Pext.Slow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "Data.Bits.PextSpec" $ do
  describe "Emulated pext and native pext give the same answer" $ do
    it "Word64" $ require $ property $ do
      a <- forAll $ G.word64 R.constantBounded
      b <- forAll $ G.word64 R.constantBounded
      pext a b === slowPext a b
    it "Word32" $ require $ property $ do
      a <- forAll $ G.word32 R.constantBounded
      b <- forAll $ G.word32 R.constantBounded
      pext a b === slowPext a b
    it "Word16" $ require $ property $ do
      a <- forAll $ G.word16 R.constantBounded
      b <- forAll $ G.word16 R.constantBounded
      pext a b === slowPext a b
    it "Word8" $ require $ property $ do
      a <- forAll $ G.word8 R.constantBounded
      b <- forAll $ G.word8 R.constantBounded
      pext a b === slowPext a b
    it "Word" $ require $ property $ do
      a <- forAll $ G.word R.constantBounded
      b <- forAll $ G.word R.constantBounded
      pext a b === slowPext a b
