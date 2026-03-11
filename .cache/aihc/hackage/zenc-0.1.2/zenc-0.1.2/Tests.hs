module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog

import Text.Encoding.Z

main :: IO ()
main = hspec $ do
  describe "Round trip" $ do
    it "self-inverse of arbitrary ascii" $ hedgehog $ do
      xs <- forAll $ Gen.string (Range.linear 0 100) Gen.ascii
      zDecodeString (zEncodeString xs) === xs

