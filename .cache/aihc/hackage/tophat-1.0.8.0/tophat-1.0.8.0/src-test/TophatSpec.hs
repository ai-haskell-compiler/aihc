{-# LANGUAGE OverloadedStrings #-}

module TophatSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Tophat


spec :: Spec
spec = do

  describe "ForContext" $ do
    let t = makeTemplate (    embedConst "a"
                          >>> forH (replicate 3)
                          >>> embed id
                          >>> endfor
                          >>> embedConst "c")
    it "runs three times" $
      runTemplate t "b" `shouldBe` ("abbbc" :: Text)

  describe "WithContext" $ do
    let t = makeTemplate (    embedConst "a"
                          >>> withH fst
                          >>> embed id
                          >>> endwith
                          >>> withH snd
                          >>> embed id
                          >>> endwith
                          >>> embedConst "d")
    it "localises" $
      runTemplate t ("b","c") `shouldBe` ("abcd" :: Text)             

  describe "IfContext" $ do
    let t = makeTemplate (    embedConst "["
                          >>> ifH (> 0)
                          >>> embedShow id
                          >>> endif
                          >>> embedConst "]")
    it "runs when true" $
      runTemplate t (3 :: Int) `shouldBe` ("[3]" :: Text)
    it "doesn't run when false" $
      runTemplate t (-2 :: Int) `shouldBe` ("[]" :: Text)

  describe "ProcContext" $ do
    let t = makeTemplate (    procH (T.replace "dog" "cat")
                          >>> embedConst "cond"
                          >>> embedConst "ogena"
                          >>> embedConst "tion"
                          >>> endproc)
    it "postprocesses" $
      runTemplate t () `shouldBe` "concatenation"
