module Main (main) where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Text.Hex (decodeHex, encodeHex, lazilyEncodeHex)
import Prelude (IO, Maybe (..), cycle, fmap, ($), (.))

main :: IO ()
main = hspec do
  encodeHexTests
  decodeHexTests
  lazilyEncodeHexTests

encodeHexTests :: Spec
encodeHexTests =
  describe "encodeHex" do
    it "can encode a single byte" do
      let f = encodeHex . ByteString.singleton
      f 192 `shouldBe` fromString "c0"
      f 168 `shouldBe` fromString "a8"
    it "can encode many bytes" do
      let f = encodeHex . ByteString.pack
      f [192, 168, 1, 2] `shouldBe` fromString "c0a80102"

decodeHexTests :: Spec
decodeHexTests =
  describe "decodeHex" do
    let f = fmap ByteString.unpack . decodeHex . Text.pack
    it "can decode" do
      f "c0a80102" `shouldBe` Just [192, 168, 1, 2]
      f "C0A80102" `shouldBe` Just [192, 168, 1, 2]
    it "can fail" do
      f "c0a8010" `shouldBe` Nothing
      f "x0a80102" `shouldBe` Nothing

lazilyEncodeHexTests :: Spec
lazilyEncodeHexTests =
  describe "lazilyEncodeHex" do
    let f = lazilyEncodeHex . LazyByteString.pack
    it "can decode part of an infinite list" $
      (LazyText.take 8 . f . cycle) [1, 2, 3] `shouldBe` fromString "01020301"
