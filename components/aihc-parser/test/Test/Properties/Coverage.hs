{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Properties.Coverage (assertCtorCoverage) where

import Data.Data (Data, dataTypeConstrs, dataTypeOf, gmapQl, isAlgType, showConstr, toConstr)
import Data.Set qualified as Set
import Test.QuickCheck (Property, cover)

assertCtorCoverage :: forall a. (Data a) => [String] -> a -> (Property -> Property)
assertCtorCoverage excluded x prop =
  let allCtors = filter (`notElem` excluded) (map showConstr (dataTypeConstrs (dataTypeOf (undefined :: a))))
      coverableCtors = allCtors
      seenCtors = usedCtors x
   in foldr ($) prop [cover 1 (ctor `Set.member` seenCtors) ctor | ctor <- coverableCtors]

usedCtors :: (Data a) => a -> Set.Set String
usedCtors x =
  let here
        | isAlgType (dataTypeOf x) = Set.singleton (showConstr (toConstr x))
        | otherwise = Set.empty
   in here <> gmapQl (<>) Set.empty usedCtors x
