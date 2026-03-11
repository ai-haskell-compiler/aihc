module Test.Sound.Synthesizer.Causal.Analysis (tests) where

import qualified Synthesizer.Causal.Analysis as AnaC
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Plain.Analysis as Ana

import Control.Arrow ((<<<), )

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Match as Match
import qualified Data.List as List

import qualified Test.QuickCheck as QC
import Test.QuickCheck (quickCheck, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


movingMedian :: (Ord a) => Int -> [a] -> [a]
movingMedian n =
   map (\xs -> List.sort xs !! div (length xs) 2) . NonEmpty.tail .
   NonEmptyC.zipWith (drop . max 0) (NonEmptyC.iterate succ (negate n)) .
   NonEmpty.inits


tests :: [(String, IO ())]
tests =
   ("deltaSigmaModulation",
      quickCheck $ \xs ->
         Match.take xs (Ana.deltaSigmaModulation xs)
         ==
         Causal.apply AnaC.deltaSigmaModulation (xs::[Rational])) :
   ("deltaSigmaModulationPositive",
      quickCheck $ \threshold xs ->
         Match.take xs (Ana.deltaSigmaModulationPositive threshold xs)
         ==
         Causal.apply
            (AnaC.deltaSigmaModulationPositive <<<
             Causal.feedConstFst threshold) (xs::[Rational])) :
   ("movingMedian",
      quickCheck $
      QC.forAll (QC.choose (1,20)) $ \n xs ->
         movingMedian n xs
         ==
         Causal.apply (AnaC.movingMedian n) (xs::[Char])) :
   []
