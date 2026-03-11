module Game.Utility where

import qualified System.Random as Rnd

import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM, liftM2)

import qualified Data.Foldable as Fold
import qualified Data.EnumMap as EnumMap
import qualified Data.Map as Map
import Data.EnumMap (EnumMap)
import Data.Map (Map)

import qualified Test.QuickCheck as QC


{- $setup
>>> import Game.Utility (Choice, mergeChoice, noChoice)
-}


nullToMaybe :: [a] -> Maybe [a]
nullToMaybe [] = Nothing
nullToMaybe s  = Just s

-- candidate for random-utility, cf. module htam:Election, markov-chain
randomSelect :: (Rnd.RandomGen g, Monad m) => [a] -> MS.StateT g m a
randomSelect items =
   liftM (items!!) $ MS.state $ Rnd.randomR (0, length items-1)


histogram :: (Ord a) => [a] -> Map a Int
histogram = Map.fromListWith (+) . map (\a -> (a,1))


-- unfortunately it is not a Monoid because mergeChoice is not associative
data Choice a = Choice (EnumMap a Int) Int
   deriving (Eq, Show)

instance (QC.Arbitrary a, Enum a) => QC.Arbitrary (Choice a) where
   arbitrary = do
      bag <-
         fmap EnumMap.fromList $ QC.listOf $
         liftM2 (,) QC.arbitrary (fmap QC.getNonNegative QC.arbitrary)
      count <- QC.choose (0, Fold.sum bag)
      return $ Choice bag count
   shrink (Choice bag count) =
      map (\(xs,c) ->
            let b = fmap abs $ EnumMap.fromList xs
            in Choice b (min c $ Fold.sum b)) $
      QC.shrink (EnumMap.toList bag, count)

noChoice :: (Enum a) => Choice a
noChoice = Choice EnumMap.empty 0

-- it is hard to test whether fullEval absorbs
{- |
prop> \a -> a == mergeChoice noChoice (a :: Choice Char)
prop> \a -> a == mergeChoice a (noChoice :: Choice Char)
prop> \a b -> mergeChoice a b == mergeChoice b (a :: Choice Char)
-}
mergeChoice :: (Enum a) => Choice a -> Choice a -> Choice a
mergeChoice (Choice symbolsA countA) (Choice symbolsB countB) =
   Choice
      (EnumMap.unionWith max symbolsA symbolsB)
      (countA + countB
         - min (min countA countB)
               (Fold.sum (EnumMap.intersectionWith min symbolsA symbolsB)))

{-
Unfortunately, this does not apply:

*Test.Mastermind EnumMap> let a = Choice (EnumMap.singleton 'x' 1) 1
*Test.Mastermind EnumMap> let b = Choice (EnumMap.singleton 'x' 1) 0
*Test.Mastermind EnumMap> let c = Choice (EnumMap.singleton 'y' 1) 1
*Test.Mastermind EnumMap> mergeChoice (mergeChoice a b) c
Choice (fromList [('x',1),('y',1)]) 2
*Test.Mastermind EnumMap> mergeChoice a (mergeChoice b c)
Choice (fromList [('x',1),('y',1)]) 1
*Test.Mastermind EnumMap> mergeChoice a b
Choice (fromList [('x',1)]) 1
*Test.Mastermind EnumMap> mergeChoice b c
Choice (fromList [('x',1),('y',1)]) 1
-}
_choiceAssociative :: Choice Char -> Choice Char -> Choice Char -> Bool
_choiceAssociative a b c =
   mergeChoice (mergeChoice a b) c
   ==
   mergeChoice a (mergeChoice b c)
