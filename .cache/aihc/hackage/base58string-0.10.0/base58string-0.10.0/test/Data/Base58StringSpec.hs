module Data.Base58StringSpec where

import           Data.Base58String.Bitcoin ( b58String
                                           , fromBytes
                                           , toBytes )

import qualified Data.ByteString.Char8 as BS8

import           Test.Hspec

spec :: Spec
spec = do
  describe "when constructing a hex string" $ do
    it "should accept strings that fall within a valid range" $
      b58String (BS8.pack "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz") `shouldBe` b58String (BS8.pack "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

    it "should reject strings outside the range" $ do
      putStrLn (show (b58String (BS8.pack "0"))) `shouldThrow` anyErrorCall
      putStrLn (show (b58String (BS8.pack ":"))) `shouldThrow` anyErrorCall
      putStrLn (show (b58String (BS8.pack "`"))) `shouldThrow` anyErrorCall
      putStrLn (show (b58String (BS8.pack "{"))) `shouldThrow` anyErrorCall
      putStrLn (show (b58String (BS8.pack "I"))) `shouldThrow` anyErrorCall
      putStrLn (show (b58String (BS8.pack "O"))) `shouldThrow` anyErrorCall
      putStrLn (show (b58String (BS8.pack "l"))) `shouldThrow` anyErrorCall

  describe "when interpreting a hex string" $ do
    it "should convert the hex string properly when interpreting as bytes" $
      toBytes (b58String (BS8.pack "15Q")) `shouldBe` BS8.pack "\0\255"
    it "should convert bytes to the proper hex string" $
      fromBytes (BS8.pack "\0\255") `shouldBe` b58String (BS8.pack "15Q")

-- ABCDEFGH JKLMN PQRSTUVWXYZabcdefghijk mnopqrstuvwxyz
-- abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz
