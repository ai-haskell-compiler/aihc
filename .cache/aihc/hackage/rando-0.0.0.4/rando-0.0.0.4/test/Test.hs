-- | It's obviously difficult to test nondeterministic functions.
--   Some of these methods are \"uncommon\" but they do the job.

{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Microspec hiding (shuffle)

import Rando

main :: IO ()
main = microspec $ do
   describe "shuffle" $ do

      -- This tests that there's no position in the list (e.g. first or last
      --   element) that's "stuck" (never changes position):
      it "at least sometimes, all elements change position" $ \(NonEmpty l) ->
         length (L.nub l) > 1 ==>
            monadicIO $ prop_sometimesAllElemsChange $ NonEmpty l

      -- This is apparently a very common symptom of off-by-one errors with
      --   this algorithm:
      it "sometimes has elements which don't change position" $
         monadicIO . prop_sometimesElemsDontChange

      it "produces roughly equal distributions" $
         monadicIO $ prop_roughlyEqualShuffleDistributions

   describe "pickOne" $ do
      it "draws from all elements of the list" $
         monadicIO . prop_pickOne_picksFromAllElems

-- L.nub so we're not confused by dupes:
prop_sometimesAllElemsChange :: NonEmptyList Int -> PropertyM IO Bool
prop_sometimesAllElemsChange (NonEmpty (L.nub -> l_u)) = do
   l_s <- run $ shuffle l_u
   if (and::[Bool]->Bool) $ zipWith (/=) l_s l_u
      then pure True
      else prop_sometimesAllElemsChange $ NonEmpty l_u

prop_sometimesElemsDontChange :: NonEmptyList Int -> PropertyM IO Bool
prop_sometimesElemsDontChange (NonEmpty (L.nub -> l_u)) = do
   l_s <- run $ shuffle l_u
   if (or::[Bool]->Bool) $ zipWith (==) l_s l_u
      then pure True
      else prop_sometimesElemsDontChange $ NonEmpty l_u

prop_roughlyEqualShuffleDistributions :: PropertyM IO Bool
prop_roughlyEqualShuffleDistributions = do
   allRuns <- run $ replicateM numRuns (shuffle ([1..9] :: [Int]))
   -- TODO: this weirdly passed when 'shuffle' was undefined! Why??
   let thingWereReallyTesting = (and::[Bool]->Bool) $
          map (closeTo5 . average) $
             L.transpose allRuns
       -- TODO: it passes even with this!!:
       forceMore =
          let (a:b:_) = allRuns
          in a /= b
   -- run $ print $ take 2 allRuns
   pure $ thingWereReallyTesting && forceMore
 where
   numRuns :: Int
   numRuns = 10000

   average :: [Int] -> Double
   average l = toEnum (sum l) / toEnum numRuns

   closeTo5 :: Double -> Bool
   closeTo5 n = abs (n - 5) < 0.02

prop_pickOne_picksFromAllElems :: Set Int -> PropertyM IO Bool
prop_pickOne_picksFromAllElems ourList =
   pickFromTest' Set.empty
 where
   pickFromTest' :: Set Int -> PropertyM IO Bool
   pickFromTest' onesWeveSeenSoFar = do
      if ourList == onesWeveSeenSoFar
         then pure True
         else do
            nextVal <- run $ pickOne $ Set.toList ourList
            pickFromTest' (Set.insert nextVal onesWeveSeenSoFar)

