module Codec.Binary.UTF8.Light.HelperSpec where

import Test.Hspec

import Codec.Binary.UTF8.Light.Helper

import GHC.Word (Word32)


main :: IO ()
main = hspec spec


spec :: Spec
spec = do

    describe "w2c" $ do
      it "converts a Word32 to a Char" $
        w2c 97 `shouldBe` 'a'

    describe "c2w" $ do
      it "converts a Char to a Word32" $
        c2w 'b' `shouldBe` 98

    describe "i2w" $ do
      it "converts an Int to a Word32" $
        i2w 98 `shouldBe` (98 :: Word32)
      -- TODO [bug] Bug in the old implementation
      --   λ> i2w 4294967296
      --   4294967296
      --   λ> 4294967296 :: Word32
      --   0

    describe "w2i" $ do
      it "converts a Word32 to an Int" $
        w2i 18 `shouldBe` (18 :: Int)

