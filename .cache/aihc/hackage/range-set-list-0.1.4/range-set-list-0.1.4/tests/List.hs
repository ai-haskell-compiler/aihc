module List (listProps) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Data.Set as Set

import           Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet

import Control.Applicative
import Data.Int

import Data.Semigroup

import SetAction

toRSet :: (Enum a, Ord a) => SetAction a -> RSet a
toRSet AEmpty               = RSet.empty
toRSet (ASingleton a)       = RSet.singleton a
toRSet (AFromList l)        = RSet.fromList l
toRSet (AInsert a set)      = RSet.insert a $ toRSet set
toRSet (ADelete a set)      = RSet.delete a $ toRSet set
toRSet (AUnion a b)         = RSet.union (toRSet a) (toRSet b)
toRSet (ADifference a b)    = RSet.difference (toRSet a) (toRSet b)
toRSet (AIntersection a b)  = RSet.intersection (toRSet a) (toRSet b)

elementsProp :: SetAction Int -> Property
elementsProp seta = Set.elems (toSet seta) === RSet.elems (toRSet seta)

sizeProp :: SetAction Int -> Property
sizeProp seta = Set.size (toSet seta) === RSet.size (toRSet seta)

nullProp :: SetAction Int -> Property
nullProp seta = Set.null (toSet seta) === RSet.null (toRSet seta)

memberProp :: Int -> SetAction Int -> Property
memberProp x seta = Set.member x (toSet seta) === RSet.member x (toRSet seta)

notMemberProp :: Int -> RSetAction Int -> Property
notMemberProp x seta = Set.notMember x (rangeToSet seta) === RSet.notMember x (rangeToRSet seta)

lookupLTProp :: Int -> RSetAction Int -> Property
lookupLTProp x seta = Set.lookupLT x (rangeToSet seta) === RSet.lookupLT x (rangeToRSet seta)

lookupGTProp :: Int -> SetAction Int -> Property
lookupGTProp x seta = Set.lookupGT x (toSet seta) === RSet.lookupGT x (toRSet seta)

lookupLEProp :: Int -> SetAction Int -> Property
lookupLEProp x seta = Set.lookupLE x (toSet seta) === RSet.lookupLE x (toRSet seta)

lookupGEProp :: Int -> RSetAction Int -> Property
lookupGEProp x seta = Set.lookupGE x (rangeToSet seta) === RSet.lookupGE x (rangeToRSet seta)

isSubsetProp :: SetAction Int -> RSetAction Int -> Property
isSubsetProp seta setb = Set.isSubsetOf (toSet seta) (rangeToSet setb) === RSet.isSubsetOf (toRSet seta) (rangeToRSet setb)

splitProp :: Int -> RSetAction Int -> Property
splitProp x seta = Set.elems sl === RSet.elems rl .&&. sm === rm .&&. Set.elems su === RSet.elems ru where
  (sl, sm, su) = Set.splitMember x (rangeToSet seta)
  (rl, rm, ru) = RSet.splitMember x (rangeToRSet seta)

rangeToRSet :: (Enum a, Ord a) => RSetAction a -> RSet a
rangeToRSet RAEmpty               = RSet.empty
rangeToRSet (RASingleton a)       = RSet.singletonRange a
rangeToRSet (RAFromList l)        = RSet.fromRangeList l
rangeToRSet (RAInsert a set)      = RSet.insertRange a $ rangeToRSet set
rangeToRSet (RADelete a set)      = RSet.deleteRange a $ rangeToRSet set
rangeToRSet (RAUnion a b)         = RSet.union (rangeToRSet a) (rangeToRSet b)
rangeToRSet (RADifference a b)    = RSet.difference (rangeToRSet a) (rangeToRSet b)
rangeToRSet (RAIntersection a b)  = RSet.intersection (rangeToRSet a) (rangeToRSet b)

rangeProp :: RSetAction Int8 -> Property
rangeProp seta = Set.elems (rangeToSet seta) === RSet.elems (rangeToRSet seta)

ordered :: Ord a => [(a,a)] -> Bool
ordered rs = all lt $ zip rs (tail rs)
  where
    lt :: Ord a => ((a,a),(a,a)) -> Bool
    lt ((_,y),(u,_)) = y < u

pairOrdered :: Ord a => [(a, a)] -> Bool
pairOrdered = all (uncurry (<=))

orderedProp :: RSetAction Int8 -> Bool
orderedProp setAction = ordered rs && pairOrdered rs
  where rs = RSet.toRangeList $ rangeToRSet $ setAction

ascListProp :: RSetAction Int8 -> Property
ascListProp setAction = RSet.fromAscList (RSet.toAscList rs) === rs
  where rs = rangeToRSet setAction

-- Complement laws
complementProps :: TestTree
complementProps = testGroup "complement"
  [ QC.testProperty "definition"   (\a e -> RSet.member e (rs a) === RSet.notMember e (RSet.complement (rs a)))
  , QC.testProperty "involutive"   (\a -> rs a === RSet.complement (RSet.complement (rs a)))
  , QC.testProperty "(full \\\\)"  (\a -> RSet.complement (rs a) === RSet.full RSet.\\ (rs a))
  ]
  where rs = rangeToRSet :: RSetAction Int -> RSet Int

-- Min/Max laws

findMinProp :: RSetAction Int8 -> Property
findMinProp seta
  | Set.null s  = label "trivial" $ property True
  | otherwise   = Set.findMin s === RSet.findMin rs
  where s   = rangeToSet seta
        rs  = rangeToRSet seta

findMaxProp :: RSetAction Int8 -> Property
findMaxProp seta
  | Set.null s  = label "trivial" $ property True
  | otherwise   = Set.findMax s === RSet.findMax rs
  where s   = rangeToSet seta
        rs  = rangeToRSet seta

minMaxProps :: TestTree
minMaxProps = testGroup "Min/Max properties"
  [ QC.testProperty "findMin"  findMinProp
  , QC.testProperty "findMax"  findMaxProp
  ]

-- Monoid laws
monoidLaws :: TestTree
monoidLaws = testGroup "Monoid laws"
  [ QC.testProperty "left identity"   (\a -> rs a === mempty <> rs a)
  , QC.testProperty "right identity"  (\a -> rs a === rs a <> mempty)
  , QC.testProperty "associativity"   (\a b c -> rs a <> (rs b <> rs c) === (rs a <> rs b) <> rs c)
  ]
  where rs = rangeToRSet :: RSetAction Int -> RSet Int

validProp :: SetAction Int -> Property
validProp s = RSet.valid (toRSet s) === True

validRProp :: RSetAction Int -> Property
validRProp s = RSet.valid (rangeToRSet s) === True

invalidProp :: Property
invalidProp = RSet.valid (RSet.fromNormalizedRangeList [(-10,-1),(1,0),(2,3 :: Int)]) === False

-- All QuickCheck properties
listProps :: TestTree
listProps = testGroup "QuickCheck List properties"
  [ QC.testProperty "element operations are similar" elementsProp
  , QC.testProperty "size is consistent" sizeProp
  , QC.testProperty "null operation is similar" nullProp
  , QC.testProperty "member operation is similar" memberProp
  , QC.testProperty "notMember operation is similar" notMemberProp
  , QC.testProperty "lookupLT operation is similar" lookupLTProp
  , QC.testProperty "lookupGT operation is similar" lookupGTProp
  , QC.testProperty "lookupLE operation is similar" lookupLEProp
  , QC.testProperty "lookupGE operation is similar" lookupGEProp
  , QC.testProperty "isSubset operation is similar" isSubsetProp
  , QC.testProperty "split operation is similar" splitProp
  , QC.testProperty "range operations is similar" rangeProp
  , QC.testProperty "ranges remain is ordered" orderedProp
  , QC.testProperty "fromAscList . toAscList === id" ascListProp
  , complementProps
  , minMaxProps
  , monoidLaws
  , QC.testProperty "item sets valid" validProp
  , QC.testProperty "range sets valid" validRProp
  , QC.testProperty "fromNormalizedRangeList invalid" invalidProp
  ]
