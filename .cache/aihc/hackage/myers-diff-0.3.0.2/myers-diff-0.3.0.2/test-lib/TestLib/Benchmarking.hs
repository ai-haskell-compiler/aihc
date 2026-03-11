{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module TestLib.Benchmarking (
  testGroup

  , getPairSingleInsert
  , getPairSingleDelete

  , getPairWithEdit

  , PairFn
  ) where

import Control.Monad
import Criterion
import qualified Data.Diff.Myers as VM
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Test.QuickCheck
import TestLib.Generators
import TestLib.Instances ()

#ifdef DIFF
import qualified Data.Diff.Diff as DD
#endif


-- * Test group

testGroup :: PairFn -> Int -> Int -> Benchmark
testGroup getPair numSamples inputSize =
  env (getPair numSamples inputSize) $ \samples -> do
    bgroup [i|#{inputSize} characters|] [
      bench "myers-diff-vector" $ nf (L.map (\(_, _, _, _, initialVector, finalVector) -> (VM.diffVectors initialVector finalVector))) samples
      , bench "myers-diff-string" $ nf (L.map (\(initialString, finalString, _, _, _, _) -> (VM.diffStrings initialString finalString))) samples
      , bench "myers-diff-text" $ nf (L.map (\(_, _, initialText, finalText, _, _) -> (VM.diffTexts initialText finalText))) samples
#ifdef DIFF
      , bench "Diff" $ nf (L.map (\(initialString, finalString, _, _, _, _) -> (DD.diff initialString finalString))) samples
#endif
      ]

-- * Pair generation

type PairFn = Int -> Int -> IO [(String, String, Text, Text, VU.Vector Char, VU.Vector Char)]

getPairWithEdit :: (Text -> Gen Text) -> PairFn
getPairWithEdit makeEdit numSamples inputSize = do
  replicateM numSamples $ do
    (t1, t2) <- generate ((resize inputSize arbitraryAlphanumericString) >>= (\initial -> (initial, ) <$> makeEdit initial))

    return (T.unpack t1, T.unpack t2
           , t1, t2
           , VM.fastTextToVector t1, VM.fastTextToVector t2
           )

getPairSingleInsert :: PairFn
getPairSingleInsert = getPairWithEdit arbitraryInsertOn

getPairSingleDelete :: PairFn
getPairSingleDelete = getPairWithEdit arbitraryDeleteOn
