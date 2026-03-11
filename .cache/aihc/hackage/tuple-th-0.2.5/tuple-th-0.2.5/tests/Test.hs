{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# OPTIONS_GHC -ddump-splices -Wall -fno-warn-name-shadowing #-}


import TupleTH
import Test.QuickCheck.All
import Data.Char
import Test.QuickCheck.Property
import Data.Maybe

prop_foldrTuple ::  (Int, Int, Int) -> Bool
prop_foldrTuple t@(x::Int,y,z) = $(foldrTuple 3) (:) [] t == [x,y,z]
prop_foldlTuple ::  (Int, Int, Int) -> Bool
prop_foldlTuple t@(x::Int,y,z) = $(foldlTuple 3) (flip (:)) [] t == [z,y,x]
prop_foldr1Tuple ::  (Int, Int, Int) -> Bool
prop_foldr1Tuple t@(x::Int,y,z) = $(foldr1Tuple 3) (-) t == x - (y - z)
prop_foldl1Tuple ::  (Int, Int, Int) -> Bool
prop_foldl1Tuple t@(x::Int,y,z) = $(foldl1Tuple 3) (-) t == (x - y) - z
prop_filterTuple ::  (Int, Int) -> Bool
prop_filterTuple t@(x::Int,y) = $(filterTuple 2) even t == filter even [x,y]
prop_mapTuple ::  (Char, Char, Char, Char) -> Bool
prop_mapTuple t@(x,y,z,a) = $(mapTuple 4) ord t == (ord x, ord y, ord z, ord a)
prop_nth ::  (Int, Int, Int) -> Bool
prop_nth t@(x::Int,y,z) = [ $(proj 3 0) t, $(proj 3 1) t, $(proj 3 2) t ] == [x,y,z]
prop_zipTuple ::  (Int, Integer) -> (Char, String) -> Bool
prop_zipTuple t@(x::Int,y::Integer) t'@(x'::Char,y'::String) =
    $(zipTuple 2) t t' == ((x,x'),(y,y'))

prop_zipTuple3 ::  (Int, Integer, Int) -> (Char, String, String) -> Bool
prop_zipTuple3 t@(x,y,z) t'@(x',y',z') =
    $(zipTuple 3) t t' == ((x,x'),(y,y'),(z,z'))

prop_tupleFromList ::  Int -> Int -> Int -> Bool
prop_tupleFromList (x::Int) y z = $(tupleFromList 3) [x,y,z] == (x,y,z) 
prop_safeTupleFromList ::  Int -> Int -> Int -> Bool
prop_safeTupleFromList (x::Int) y z = $(safeTupleFromList 3) [x,y,z] == Just (x,y,z)
prop_safeTupleFromList_tooLarge ::  Int -> Int -> Int -> Bool
prop_safeTupleFromList_tooLarge (x::Int) y z = $(safeTupleFromList 2) [x,y,z] == Nothing
prop_safeTupleFromList_tooSmall ::  Int -> Int -> Int -> Bool
prop_safeTupleFromList_tooSmall (x::Int) y z = $(safeTupleFromList 4) [x,y,z] == Nothing

prop_elemTuple ::  Int -> Int -> Int -> Int -> Int -> Bool
prop_elemTuple (x::Int) a b c d = $(elemTuple 4) x (a,b,c,d) == elem x [a,b,c,d] 

prop_reindexTuple :: Int -> Int -> Int -> Bool
prop_reindexTuple (x::Int) y z = $(reindexTuple 3 [1,1,0,0]) (x,y,z) == (y,y,x,x)

prop_deleteAtTuple :: Int -> Int -> Int -> Property
prop_deleteAtTuple x y z = 
        (deleteAtTuple3 0 t == (y,z) )
    .&. (deleteAtTuple3 1 t == (x,z) )
    .&. (deleteAtTuple3 2 t == (x,y) )
  where
    t = (x,y,z)


deleteAtTuple3 :: Integer -> (Int,Int,Int) -> (Int,Int)
deleteAtTuple3 = $(deleteAtTuple 3)

prop_deleteAtTuple_oob1 :: Property
prop_deleteAtTuple_oob1 = expectFailure (deleteAtTuple3 (-1) (1,2,3) `seq` True)
prop_deleteAtTuple_oob2 :: Property
prop_deleteAtTuple_oob2 = expectFailure (deleteAtTuple3 3 (1,2,3) `seq` True)


prop_proj' :: (Int,Int,Int) -> Property
prop_proj' xs@(x,y,z) = 
    let 
        xs' = $(mapTuple 5) (\i -> $(proj' 3) i xs) (-1::Int,0,1,2,3)
    in
        property (xs' == (Nothing,Just x,Just y,Just z,Nothing))


main ::  IO Bool
main = $(quickCheckAll)
