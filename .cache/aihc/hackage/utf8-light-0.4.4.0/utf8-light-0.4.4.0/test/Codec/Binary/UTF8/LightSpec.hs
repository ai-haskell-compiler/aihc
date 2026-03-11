module Codec.Binary.UTF8.LightSpec where

import Test.Hspec

import Codec.Binary.UTF8.Light


main :: IO ()
main = hspec spec


spec :: Spec
spec = do

    describe "encodeUTF8'" $ do
      it "encodes UTF-8" $
        encodeUTF8' [9212 :: Word32] `shouldBe` [[226,143,188] :: [Word8]]
      it "drops Word32 representing invalid UTF-8" $
        encodeUTF8' [921234567 :: Word32] `shouldBe` [[] :: [Word8]]

