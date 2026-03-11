{-# LANGUAGE BangPatterns #-}

import           Control.Loop (numLoop)
import           Control.Monad
import           Data.Word
import           Test.Hspec

import qualified Data.ReinterpretCast as Current
import qualified Data.ReinterpretCast.Internal.ImplArray as Array
import qualified Data.ReinterpretCast.Internal.ImplFFI as FFI
-- Keep comparing against the data-binary-ieee754 package in case that changes.
import qualified Data.Binary.IEEE754 as IEEE


main :: IO ()
main = hspec $ do

  describe "Current" $ do

    it "wordToFloat" $ do
      Current.wordToFloat 1 `shouldBe` 1.0e-45


  describe "exhaustive testing (~ 2 minutes per test)" $ do

    it "Word32 / float" $ do
      numLoop 0 (maxBound :: Word32) $ \w ->
        when (Current.floatToWord (Current.wordToFloat w) /= w) $
          expectationFailure $ "Failed: " ++ show w


  describe "Comparing implementations" $ do

    let n32 = 1000000 :: Word32

    it ("[0.." ++ show n32 ++ "] Word32 / float") $ do
      numLoop 0 n32 $ \w -> do
        let f = IEEE.wordToFloat w
        when (f /= Current.wordToFloat w) $ expectationFailure $ "Current failed: " ++ show w
        when (f /= Array.wordToFloat w)   $ expectationFailure $ "Array failed: " ++ show w
        when (f /= FFI.wordToFloat w)     $ expectationFailure $ "FFI failed: " ++ show w


    let n64 = 1000000 :: Word64

    it ("[0.." ++ show n64 ++ "] Word64 / double") $ do
      numLoop 0 n64 $ \w -> do
        let f = IEEE.wordToDouble w
        when (f /= Current.wordToDouble w) $ expectationFailure $ "Current failed: " ++ show w
        when (f /= Array.wordToDouble w)   $ expectationFailure $ "Array failed: " ++ show w
        when (f /= FFI.wordToDouble w)     $ expectationFailure $ "FFI failed: " ++ show w
