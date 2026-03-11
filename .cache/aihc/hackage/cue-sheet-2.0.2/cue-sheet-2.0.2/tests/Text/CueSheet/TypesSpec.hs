{-# LANGUAGE OverloadedStrings #-}

module Text.CueSheet.TypesSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Text.CueSheet.Types

spec :: Spec
spec =
  describe "fromMmSsFf" $ do
    context "if number of seconds is greater than 59" $
      it "throws the correct exception" $
        property $ \(NonNegative mm') (NonNegative ss') (NonNegative ff') -> do
          let mm = fromInteger mm'
              ss = fromInteger ss' + 60
              ff = fromInteger ff'
          fromMmSsFf mm ss ff `shouldThrow` (== InvalidSeconds ss)
    context "if number of frames is greater than 74" $
      it "throws the correct exception" $
        property $ \(NonNegative mm') (NonNegative ss') (NonNegative ff') -> do
          let mm = fromInteger mm'
              ss = fromInteger ss' `rem` 60
              ff = fromInteger ff' + 75
          fromMmSsFf mm ss ff `shouldThrow` (== InvalidFrames ff)
    context "if all input values are valid" $
      it "produces the correct result" $ do
        fromMmSsFf 0 13 66 `shouldReturn` CueTime 1041
        fromMmSsFf 3 44 10 `shouldReturn` CueTime 16810
        fromMmSsFf 9 0 0 `shouldReturn` CueTime 40500
    describe "showMmSsFf" $
      it "works correctly" $ do
        let f mm ss ff = showMmSsFf <$> fromMmSsFf mm ss ff
        f 0 12 3 `shouldReturn` "00:12:03"
        f 9 17 73 `shouldReturn` "09:17:73"
        f 99 59 74 `shouldReturn` "99:59:74"
        f 100 0 35 `shouldReturn` "100:00:35"
    describe "mkMcn" $ do
      context "when input value is a valid MCN" $
        it "returns the MCN all right" $
          property $ \mcn ->
            mkMcn (unMcn mcn) `shouldReturn` mcn
      context "when input value is invalid" $
        it "throws the correct exception" $ do
          mkMcn "something" `shouldThrow` (== InvalidMcn "something")
          mkMcn "123123123" `shouldThrow` (== InvalidMcn "123123123")
          mkMcn "111222333444a" `shouldThrow` (== InvalidMcn "111222333444a")
    describe "mkCueText" $ do
      context "when input value is a valid CUE text" $
        it "returns the CueText all right" $
          property $ \cueText ->
            mkCueText (unCueText cueText) `shouldReturn` cueText
      context "when input value is invalid" $ do
        context "when it's too short" $
          it "throws the correct exception" $
            mkCueText "" `shouldThrow` (== InvalidCueText "")
        context "when it's too long" $
          it "throws the correct exception" $ do
            let tooLong = T.replicate 81 "a"
            mkCueText tooLong `shouldThrow` (== InvalidCueText tooLong)
        context "when it contains the \" character" $
          it "throws the correct exception" $
            mkCueText "re\"re" `shouldThrow` (== InvalidCueText "re\"re")
        context "when it contains the newline character" $
          it "throws the correct exception" $
            mkCueText "re\nre" `shouldThrow` (== InvalidCueText "re\nre")
    describe "mkIsrc" $ do
      context "when input value is a valid ISRC" $
        it "returns the ISRC all right" $
          property $ \isrc ->
            mkIsrc (unIsrc isrc) `shouldReturn` isrc
      context "when input value is invalid" $
        it "throws the correct exception" $ do
          mkIsrc "a" `shouldThrow` (== InvalidIsrc "a")
          mkIsrc "12323asoent8" `shouldThrow` (== InvalidIsrc "12323asoent8")
          mkIsrc "aoe8u120997u" `shouldThrow` (== InvalidIsrc "aoe8u120997u")
