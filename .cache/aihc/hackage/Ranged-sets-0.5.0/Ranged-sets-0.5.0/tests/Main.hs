{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Ranged
import Test.HUnit
import Test.QuickCheck


conf :: Args
conf = stdArgs { maxSuccess = 1000, maxDiscardRatio = 100 }

check :: (Test.QuickCheck.Testable prop) => prop -> IO ()
check = quickCheckWith conf

main :: IO ()
main = do
   putStrLn "QuickCheck Data.Ranged.Ranges:"
   putStrLn "   Sparse type Integer:"
   putStrLn "      * prop_unionRange"
   check $ \(r1 :: Range Integer) -> prop_unionRange r1
   putStrLn "      * prop_unionRangeLength"
   check $ \(r1 :: Range Integer) -> prop_unionRangeLength r1
   putStrLn "      * prop_intersectionRange"
   check $ \(r1 :: Range Integer) -> prop_intersectionRange r1
   putStrLn "      * prop_intersectionOverlap"
   check $ \(r1 :: Range Integer) -> prop_intersectionOverlap r1
   putStrLn "      * prop_enclosureUnion"
   check $ \(r1 :: Range Integer) -> prop_enclosureUnion r1
   putStrLn "      * prop_differenceRange"
   check $ \(r1 :: Range Integer) -> prop_differenceRange r1
   putStrLn "      * prop_singletonRangeHas"
   check $ \(v :: Integer) -> prop_singletonRangeHas v
   putStrLn "      * prop_singletonRangeHasOnly"
   check $ \(v :: Integer) -> prop_singletonRangeHasOnly v
   putStrLn "      * prop_singletonRangeConverse"
   check $ \(v :: Integer) -> prop_singletonRangeConverse v

   putStrLn "   Dense type Double:"
   putStrLn "      * prop_unionRange"
   check $ \(r1 :: Range Double) -> prop_unionRange r1
   putStrLn "      * prop_unionRangeLength"
   check $ \(r1 :: Range Double) -> prop_unionRangeLength r1
   putStrLn "      * prop_intersectionRange"
   check $ \(r1 :: Range Double) -> prop_intersectionRange r1
   putStrLn "      * prop_intersectionOverlap"
   check $ \(r1 :: Range Integer) -> prop_intersectionOverlap r1
   putStrLn "      * prop_enclosureUnion"
   check $ \(r1 :: Range Integer) -> prop_enclosureUnion r1
   putStrLn "      * prop_differenceRange"
   check $ \(r1 :: Range Double) -> prop_differenceRange r1
   putStrLn "      * prop_singletonRangeHas"
   check $ \(v :: Double) -> prop_singletonRangeHas v
   putStrLn "      * prop_singletonRangeHasOnly"
   check $ \(v :: Double) -> prop_singletonRangeHasOnly v
   putStrLn "      * prop_singletonRangeConverse"
   check $ \(v :: Double) -> prop_singletonRangeConverse v

   putStrLn "   Type-insensitive tests:"
   putStrLn "      * prop_emptyNonSingleton"
   check prop_emptyNonSingleton
   putStrLn "      * prop_fullNonSingleton"
   check prop_fullNonSingleton
   putStrLn "      * prop_nonSingleton"
   check prop_nonSingleton
   putStrLn "      * prop_intSingleton"
   check prop_intSingleton

   putStrLn "   Checking show for range:"
   _ <- runTestTT $ TestList
       [
        TestCase $ assertEqual "Show range1" "3 <= x <= 8" $
                 show $ Range (BoundaryBelow (3 :: Int)) (BoundaryAbove 8),
        TestCase $ assertEqual "Show range2" "x < 8" $
                 show $ Range (BoundaryBelowAll) (BoundaryBelow (8 :: Int)),
        TestCase $ assertEqual "Show range3" "3 < x" $
                 show $ Range (BoundaryAbove (3 :: Int)) (BoundaryAboveAll),
        TestCase $ assertEqual "Show singleton" "x == 4" $
                 show $ singletonRange (4 :: Int),
        TestCase $ assertEqual "Show full" "All x" $
                 show (fullRange :: Range Int),
        TestCase $ assertEqual "Show empty" "Empty" $
                 show (emptyRange :: Range Int)
       ]

   putStrLn "QuickCheck Data.Ranged.RangedSet:"
   putStrLn "   Sparse type Integer:"
   putStrLn "      * prop_validNormalised"
   check $ \(rs :: [Range Integer]) -> prop_validNormalised rs
   putStrLn "      * prop_has"
   check $ \(rs :: [Range Integer]) -> prop_has rs
   putStrLn "      * prop_unfold"
   check prop_unfold
   putStrLn "      * prop_union"
   check $ \(rset1 :: RSet Integer) -> prop_union rset1
   putStrLn "      * prop_intersection"
   check $ \(rset1 :: RSet Integer) -> prop_intersection rset1
   putStrLn "      * prop_difference"
   check $ \(rset1 :: RSet Integer) -> prop_difference rset1
   putStrLn "      * prop_negation"
   check $ \(rset1 :: RSet Integer) -> prop_negation rset1
   putStrLn "      * prop_not_empty"
   check $ \(rset1 :: RSet Integer) -> prop_not_empty rset1
   putStrLn "      * prop_empty"
   check $ \(v :: Integer) -> prop_empty v
   putStrLn "      * prop_full"
   check $ \(v :: Integer) -> prop_full v
   putStrLn "      * prop_empty_intersection"
   check $ \(rset1 :: RSet Integer) -> prop_empty_intersection rset1
   putStrLn "      * prop_full_union"
   check $ \(rset1 :: RSet Integer) -> prop_full_union rset1
   putStrLn "      * prop_union_superset"
   check $ \(rset1 :: RSet Integer) -> prop_union_superset rset1
   putStrLn "      * prop_intersection_subset"
   check $ \(rset1 :: RSet Integer) -> prop_intersection_subset rset1
   putStrLn "      * prop_diff_intersect"
   check $ \(rset1 :: RSet Integer) -> prop_diff_intersect rset1
   putStrLn "      * prop_subset"
   check $ \(rset1 :: RSet Integer) -> prop_subset rset1
   putStrLn "      * prop_strict_subset"
   check $ \(rset1 :: RSet Integer) -> prop_strict_subset rset1
   putStrLn "      * prop_union_strict_superset"
   check $ \(rset1 :: RSet Integer) -> prop_union_strict_superset rset1
   putStrLn "      * prop_intersection_commutes"
   check $ \(rset1 :: RSet Integer) -> prop_intersection_commutes rset1
   putStrLn "      * prop_union_commutes"
   check $ \(rset1 :: RSet Integer) -> prop_union_commutes rset1
   putStrLn "      * prop_intersection_associates"
   check $ \(rset1 :: RSet Integer) -> prop_intersection_associates rset1
   putStrLn "      * prop_union_associates"
   check $ \(rset1 :: RSet Integer) -> prop_union_associates rset1
   putStrLn "      * prop_de_morgan_intersection"
   check $ \(rset1 :: RSet Integer) -> prop_de_morgan_intersection rset1
   putStrLn "      * prop_de_morgan_union"
   check $ \(rset1 :: RSet Integer) -> prop_de_morgan_union rset1

   putStrLn "   Dense type Double:"
   putStrLn "      * prop_validNormalised"
   check $ \(rs :: [Range Double]) -> prop_validNormalised rs
   putStrLn "      * prop_has"
   check $ \(rs :: [Range Double]) -> prop_has rs
   putStrLn "      * prop_unfold"
   check prop_unfold
   putStrLn "      * prop_union"
   check $ \(rset1 :: RSet Double) -> prop_union rset1
   putStrLn "      * prop_intersection"
   check $ \(rset1 :: RSet Double) -> prop_intersection rset1
   putStrLn "      * prop_difference"
   check $ \(rset1 :: RSet Double) -> prop_difference rset1
   putStrLn "      * prop_negation"
   check $ \(rset1 :: RSet Double) -> prop_negation rset1
   putStrLn "      * prop_not_empty"
   check $ \(rset1 :: RSet Double) -> prop_not_empty rset1
   putStrLn "      * prop_empty"
   check $ \(v :: Double) -> prop_empty v
   putStrLn "      * prop_full"
   check $ \(v :: Double) -> prop_full v
   putStrLn "      * prop_empty_intersection"
   check $ \(rset1 :: RSet Double) -> prop_empty_intersection rset1
   putStrLn "      * prop_full_union"
   check $ \(rset1 :: RSet Double) -> prop_full_union rset1
   putStrLn "      * prop_union_superset"
   check $ \(rset1 :: RSet Double) -> prop_union_superset rset1
   putStrLn "      * prop_intersection_subset"
   check $ \(rset1 :: RSet Double) -> prop_intersection_subset rset1
   putStrLn "      * prop_diff_intersect"
   check $ \(rset1 :: RSet Double) -> prop_diff_intersect rset1
   putStrLn "      * prop_subset"
   check $ \(rset1 :: RSet Double) -> prop_subset rset1
   putStrLn "      * prop_strict_subset"
   check $ \(rset1 :: RSet Double) -> prop_strict_subset rset1
   putStrLn "      * prop_union_strict_superset"
   check $ \(rset1 :: RSet Double) -> prop_union_strict_superset rset1
   putStrLn "      * prop_intersection_commutes"
   check $ \(rset1 :: RSet Double) -> prop_intersection_commutes rset1
   putStrLn "      * prop_union_commutes"
   check $ \(rset1 :: RSet Double) -> prop_union_commutes rset1
   putStrLn "      * prop_intersection_associates"
   check $ \(rset1 :: RSet Double) -> prop_intersection_associates rset1
   putStrLn "      * prop_union_associates"
   check $ \(rset1 :: RSet Double) -> prop_union_associates rset1
   putStrLn "      * prop_de_morgan_intersection"
   check $ \(rset1 :: RSet Double) -> prop_de_morgan_intersection rset1
   putStrLn "      * prop_de_morgan_union"
   check $ \(rset1 :: RSet Double) -> prop_de_morgan_union rset1

