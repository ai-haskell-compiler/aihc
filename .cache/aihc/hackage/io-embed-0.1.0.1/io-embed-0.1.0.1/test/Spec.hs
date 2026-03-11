{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Data.IOEmbed (embedIO)
import Data.ByteString (ByteString)

main :: IO ()
main = hspec $ do
  describe "Testing for all embeddable types except Bytes" $ do
    it "embeddable types" $ do
      $(embedIO $ return 'a') `shouldBe` 'a'
      $(embedIO $ return ("Hello world!" :: String)) `shouldBe` ("Hello world!" :: String)
      $(embedIO $ return (1 :: Integer))             `shouldBe` (1 :: Integer)
      $(embedIO $ return (1 :: Rational))            `shouldBe` (1 :: Rational)
      $(embedIO $ return ("Hello binary world!" :: ByteString)) `shouldBe` "Hello binary world!"
