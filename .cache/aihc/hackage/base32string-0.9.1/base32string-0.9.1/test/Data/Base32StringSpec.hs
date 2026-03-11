module Data.Base32StringSpec where

import           Data.Base32String.Default ( b32String
                                           , fromBytes
                                           , toBytes )

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

import           Test.Hspec

spec :: Spec
spec = do
  describe "when constructing a hex string" $ do
    it "should accept strings that fall within a valid range" $
      b32String (BS8.pack "LPB2B3KOSMDI53DG") `shouldBe` b32String (BS8.pack "LPB2B3KOSMDI53DG")

    it "should reject strings outside the range" $ do
      putStrLn (show (b32String (BS8.pack "1"))) `shouldThrow` anyErrorCall
      putStrLn (show (b32String (BS8.pack "8"))) `shouldThrow` anyErrorCall
      putStrLn (show (b32String (BS8.pack "@"))) `shouldThrow` anyErrorCall
      putStrLn (show (b32String (BS8.pack "["))) `shouldThrow` anyErrorCall

  describe "when interpreting a hex string" $ do
    it "should convert the hex string properly when interpreting as bytes" $
      toBytes (b32String (BS8.pack "AH7")) `shouldBe` BS8.pack "\0\255"
    it "should convert bytes to the proper hex string" $
      fromBytes (BS8.pack "\0\255") `shouldBe` b32String (BS8.pack "AH7")
    it "should be less bytes than the original" $
      BS.length (toBytes (b32String (BS8.pack "LPB2B3KOSMDI53DG"))) `shouldSatisfy` (<= BS.length (BS8.pack "LPB2B3KOSMDI53DG"))
