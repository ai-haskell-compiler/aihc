{-# LANGUAGE OverloadedStrings #-}

module TextSpec where

import Data.Text.Lazy (Text)
import Test.Hspec
import Text


spec :: Spec
spec = do

  describe "breakOnOnce" $ do

    it "breaks at the first appearance" $
      breakOnOnce "ss" "Mississippi" `shouldBe` Just ("Mi", "ssissippi")

    it "doesn't break if there is no appearance" $
      breakOnOnce "ss" "Connecticut" `shouldBe` Nothing

  describe "splitOnOnce" $ do

    it "splits at the first appearance" $
      splitOnOnce "ss" "Mississippi" `shouldBe` Just ("Mi", "issippi")

    it "doesn't split if there is no appearance" $
      splitOnOnce "ss" "Connecticut" `shouldBe` Nothing

  describe "splits" $ do

    it "splits \"\" correctly" $
      splits "" `shouldBe` []
    
    it "splits \"cat\" correctly" $
      splits "cat" `shouldBe` [('c', ("", "at")),
                               ('a', ("c", "t")),
                               ('t', ("ca", ""))]

  describe "textLiteral" $

    it "renders literals" $
      textLiteral "cat" `shouldBe` ("\"cat\"" :: Text)
