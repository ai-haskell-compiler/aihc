module SetAction where

import Test.Tasty.QuickCheck as QC

import           Data.Set (Set)
import qualified Data.Set as Set

import Control.Applicative

data SetAction a = AEmpty
                 | ASingleton a
                 | AFromList [a]
                 | AInsert a (SetAction a)
                 | ADelete a (SetAction a)
                 | AUnion (SetAction a) (SetAction a)
                 | ADifference (SetAction a) (SetAction a)
                 | AIntersection (SetAction a) (SetAction a)
  deriving (Show)

instance Arbitrary a => Arbitrary (SetAction a) where
  arbitrary = sized arbitrary'
    where arbitrary' n
            | n <= 0     = oneof [pure AEmpty, ASingleton <$> arbitrary]
            | otherwise  = oneof [ pure AEmpty
                                 , ASingleton <$> arbitrary
                                 , AFromList <$> arbitrary
                                 , AInsert <$> arbitrary <*> arbitrary1
                                 , ADelete <$> arbitrary <*> arbitrary1
                                 , AUnion <$> arbitrary2 <*> arbitrary2
                                 , ADifference <$> arbitrary2 <*> arbitrary2
                                 , AIntersection <$> arbitrary2 <*> arbitrary2
                                 ]
                              where arbitrary1 = arbitrary' $ n - 1
                                    arbitrary2 = arbitrary' $ n `div` 2

toSet :: (Ord a) => SetAction a -> Set a
toSet AEmpty               = Set.empty
toSet (ASingleton a)       = Set.singleton a
toSet (AFromList l)        = Set.fromList l
toSet (AInsert a set)      = Set.insert a $ toSet set
toSet (ADelete a set)      = Set.delete a $ toSet set
toSet (AUnion a b)         = Set.union (toSet a) (toSet b)
toSet (ADifference a b)    = Set.difference (toSet a) (toSet b)
toSet (AIntersection a b)  = Set.intersection (toSet a) (toSet b)

data RSetAction a = RAEmpty
                  | RASingleton (a, a)
                  | RAFromList [(a, a)]
                  | RAInsert (a, a) (RSetAction a)
                  | RADelete (a, a) (RSetAction a)
                  | RAUnion (RSetAction a) (RSetAction a)
                  | RADifference (RSetAction a) (RSetAction a)
                  | RAIntersection (RSetAction a) (RSetAction a)
  deriving (Show)

instance Arbitrary a => Arbitrary (RSetAction a) where
  arbitrary = sized arbitrary'
    where arbitrary' n
            | n <= 0     = oneof [pure RAEmpty, RASingleton <$> arbitrary]
            | otherwise  = oneof [ pure RAEmpty
                                 , RASingleton <$> arbitrary
                                 , RAFromList <$> arbitrary
                                 , RAInsert <$> arbitrary <*> arbitrary1
                                 , RADelete <$> arbitrary <*> arbitrary1
                                 , RAUnion <$> arbitrary2 <*> arbitrary2
                                 , RADifference <$> arbitrary2 <*> arbitrary2
                                 , RAIntersection <$> arbitrary2 <*> arbitrary2
                                 ]
                              where arbitrary1 = arbitrary' $ n - 1
                                    arbitrary2 = arbitrary' $ n `div` 2

rangeToSet :: (Enum a, Ord a) => RSetAction a -> Set a
rangeToSet RAEmpty               = Set.empty
rangeToSet (RASingleton a)       = Set.fromList $ uncurry enumFromTo a
rangeToSet (RAFromList l)        = Set.fromList $ concatMap (uncurry enumFromTo) l
rangeToSet (RAInsert a set)      = foldr Set.insert (rangeToSet set) $ uncurry enumFromTo a
rangeToSet (RADelete a set)      = foldr Set.delete (rangeToSet set) $ uncurry enumFromTo a
rangeToSet (RAUnion a b)         = Set.union (rangeToSet a) (rangeToSet b)
rangeToSet (RADifference a b)    = Set.difference (rangeToSet a) (rangeToSet b)
rangeToSet (RAIntersection a b)  = Set.intersection (rangeToSet a) (rangeToSet b)

