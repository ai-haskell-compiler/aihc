{-# LANGUAGE CPP #-}

module Main (main) where

import qualified Data.Diff.Myers as VM
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector.Unboxed as VU
import TestLib.Benchmarking
import TestLib.Instances ()
import Weigh

#ifdef DIFF
import qualified Data.Diff.Diff as DD
#endif


testFunc :: Int -> [(String, String, Text, Text, VU.Vector Char, VU.Vector Char)] -> Weigh ()
testFunc inputSize samples = wgroup [i|#{inputSize} characters|] $ do
  func' "myers-diff-vector" (L.map (\(_, _, _, _, initialVector, finalVector) -> (VM.diffVectors initialVector finalVector))) samples
  func' "myers-diff-string" (L.map (\(initialString, finalString, _, _, _, _) -> (VM.diffStrings initialString finalString))) samples
  func' "myers-diff-text" (L.map (\(_, _, initialText, finalText, _, _) -> (VM.diffTexts initialText finalText))) samples
#ifdef DIFF
  func' "Diff" (L.map (\(initialString, finalString, _, _, _, _) -> (DD.diff initialString finalString))) samples
#endif

main :: IO ()
main = do
  insertSamples10 <- getPairSingleInsert 100 10
  insertSamples100 <- getPairSingleInsert 100 100
  insertSamples1000 <- getPairSingleInsert 100 1000
  insertSamples10000 <- getPairSingleInsert 100 10000
  insertSamples100000 <- getPairSingleInsert 100 100000

  deleteSamples10 <- getPairSingleDelete 100 10
  deleteSamples100 <- getPairSingleDelete 100 100
  deleteSamples1000 <- getPairSingleDelete 100 1000
  deleteSamples10000 <- getPairSingleDelete 100 10000
  deleteSamples100000 <- getPairSingleDelete 100 100000

  mainWith $ do
    setFormat Markdown

    wgroup [i|Single insert (100 samples each)|] $ do
      testFunc 10 insertSamples10
      testFunc 100 insertSamples100
      testFunc 1000 insertSamples1000
      testFunc 10000 insertSamples10000
      testFunc 100000 insertSamples100000

    wgroup [i|Single delete (100 samples each)|] $ do
      testFunc 10 deleteSamples10
      testFunc 100 deleteSamples100
      testFunc 1000 deleteSamples1000
      testFunc 10000 deleteSamples10000
      testFunc 100000 deleteSamples100000
