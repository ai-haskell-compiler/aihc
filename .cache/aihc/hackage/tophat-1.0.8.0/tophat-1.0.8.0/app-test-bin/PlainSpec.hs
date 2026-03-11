{-# OPTIONS_GHC -F -pgmF "tophat" #-}
{-# LANGUAGE OverloadedStrings #-}

module PlainSpec where

import Test.Hspec
import Data.Text
import Tophat

template :: Template Int Text
template = {{app-test-bin/plain.tpt.txt}}

spec :: Spec
spec = do
  describe "example" $ do
    it "works on n=3" $
      runTemplate template 3 `shouldBe` "We have some numbers: {1} {2} {3}.\n"
 
