{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Properties.Coverage (assertAnyCtorCoverage, assertCtorCoverage) where

import Data.Data (Data, dataTypeConstrs, dataTypeOf, gmapQl, isAlgType, showConstr, toConstr)
import Data.Set qualified as Set
import Test.QuickCheck (Property, cover)

assertCtorCoverage :: forall a. (Data a) => [String] -> a -> (Property -> Property)
assertCtorCoverage excluded x prop =
  let allCtors = filter (`notElem` excluded) (map showConstr (dataTypeConstrs (dataTypeOf (undefined :: a))))
      coverableCtors = allCtors
      seenCtors = usedCtors x
   in foldr ($) prop [cover 1 (ctor `Set.member` seenCtors) ctor | ctor <- coverableCtors]

assertAnyCtorCoverage :: forall a. (Data a) => [String] -> [a] -> (Property -> Property)
assertAnyCtorCoverage excluded xs prop =
  let allCtors = filter (`notElem` excluded) (map showConstr (dataTypeConstrs (dataTypeOf (undefined :: a))))
      seenCtors = Set.fromList (map (showConstr . toConstr) xs)
   in foldr ($) prop [cover 1 (ctor `Set.member` seenCtors) ctor | ctor <- allCtors]

usedCtors :: (Data a) => a -> Set.Set String
usedCtors x =
  let here
        | isAlgType (dataTypeOf x) = Set.singleton (showConstr (toConstr x))
        | otherwise = Set.empty
   in here <> gmapQl (<>) Set.empty usedCtors x
