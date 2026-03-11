{-# LANGUAGE MagicHash #-}

import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Ord (comparing)
import Data.Primitive.Array
  ( MutableArray(..), arrayFromList, runArray, sizeofArray, thawArray )
import Data.Primitive.PrimArray
  ( MutablePrimArray(..)
  , primArrayFromList
  , primArrayToList
  , runPrimArray
  , sizeofPrimArray
  , thawPrimArray
  )

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests(..), Fun, applyFun, testProperty, (===))
import Test.QuickCheck.Poly (A, OrdA)

import Data.SamSort (sortArrayBy, sortIntArrayBy)

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 5000) $ testGroup "Tests"
  [ testProperty "sortArrayBy" $ \xs ys zs ->
      sortViaMutableArray (comparing fst) (xs,ys,zs)
      ===
      ((xs :: [(OrdA, A)]) ++ L.sortBy (comparing fst) ys ++ zs)
  , testProperty "sortIntArrayBy" $ \f xs ys zs ->
      sortViaMutableIntArray
        (comparing (applyFun (f :: Fun Int OrdA)))
        (xs,ys,zs)
      ===
      (xs ++ L.sortBy (comparing (applyFun f)) ys ++ zs)
  ]

sortViaMutableArray
  :: (a -> a -> Ordering)
  -> ([a], [a], [a])
  -> [a]
sortViaMutableArray cmp (xs,ys,zs) = F.toList $ runArray $ do
  let a = arrayFromList (xs ++ ys ++ zs)
  ma@(MutableArray ma#) <- thawArray a 0 (sizeofArray a)
  sortArrayBy cmp ma# (length xs) (length ys)
  pure ma

sortViaMutableIntArray
  :: (Int -> Int -> Ordering)
  -> ([Int], [Int], [Int])
  -> [Int]
sortViaMutableIntArray cmp (xs,ys,zs) = primArrayToList $ runPrimArray $ do
  let a = primArrayFromList (xs ++ ys ++ zs)
  ma@(MutablePrimArray ma#) <- thawPrimArray a 0 (sizeofPrimArray a)
  sortIntArrayBy cmp ma# (length xs) (length ys)
  pure ma
