{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import           Data.Text            (Text)
import           Test.Hspec

import           System.Nagios.Plugin

main :: IO ()
main = hspec suite

suite :: Spec
suite = describe "runNagiosPlugin'" $ do
    it "returns default result if no results are specified" $ do
        (_,s) <- runNagiosPlugin' (return ())
        snd (finishState s) `shouldBe` "UNKNOWN: no check result specified"
        fst (finishState s) `shouldBe` Unknown

    it "returns explicitly added result in favor of default" $ do
        (_,s) <- runNagiosPlugin' (universeCheck pi)
        snd (finishState s) `shouldBe` ("OK: " <> universeGoodResult)
        fst (finishState s) `shouldBe` OK

    it "returns result with greatest badness" $ do
        (_,s) <- runNagiosPlugin' (universeCheck 1.0)
        snd (finishState s) `shouldBe` ("CRITICAL: " <> universeBadResult)
        fst (finishState s) `shouldBe` Critical


universeCheck :: Double -> NagiosPlugin ()
universeCheck pi' = do
    addResult OK universeGoodResult
    if (pi' < (3.0 :: Double)) then addResult Critical universeBadResult else return ()

universeGoodResult :: Text
universeGoodResult = "universe passes basic consistency tests"

universeBadResult :: Text
universeBadResult = "universe broken, π < 3"
