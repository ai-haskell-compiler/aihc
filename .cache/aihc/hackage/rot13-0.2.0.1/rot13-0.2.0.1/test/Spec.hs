{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.ByteString    as BS
import qualified Data.Text          as T

import           Codec.Rot13

main :: IO ()
main = hspec $ do
  describe "Codec.Rot13" $ do

    it "correctly ciphers the alphabet (rot13 String)" $ do
      rot13 ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" :: String)
        `shouldBe` "NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm"

    it "correctly ciphers the alphabet (rot13 ByteString)" $ do
      rot13 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        `shouldBe` ("NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm" :: BS.ByteString)

    it "correctly ciphers the alphabet (rot13 Text)" $ do
      rot13 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        `shouldBe` ("NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm" :: T.Text)

    it "is reversible (String)" $
      property $ \s -> s == (rot13 . rot13 :: String -> String) s

    it "is reversible (ByteString)" $
      property $ \s -> let s' = BS.pack s
                        in s' == (rot13 . rot13 :: BS.ByteString -> BS.ByteString) s'

    it "is reversible (Text)" $
      property $ \s -> let s' = T.pack s
                        in s' == (rot13 . rot13 :: T.Text -> T.Text) s'
