{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
--   Module      :  Data.Stack.CircularSpec
--   Description :  Unit tests for Data.Stack.Circular
--   Copyright   :  2021 Dominik Schrempf
--   License     :  GPL-3.0-or-later
--
--   Maintainer  :  dominik.schrempf@gmail.com
--   Stability   :  unstable
--   Portability :  portable
--
-- Creation date: Thu Jun 18 10:21:28 2020.
module Data.Stack.CircularSpec
  ( spec,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Aeson
import Data.List
import qualified Data.Stack.Circular as C
import qualified Data.Vector as VB
-- import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Result, Success)
import Test.QuickCheck.Instances.Vector ()
import Prelude hiding (product, sum)

instance Arbitrary (C.Stack VB.Vector Int) where
  arbitrary = do
    s <- getSize
    n <- choose (1, s + 1)
    v <- VB.fromList <$> vector n
    i <- choose (0, n - 1)
    return $ runST $ C.fromVectorWithIndex i v >>= C.freeze

se :: PrimMonad m => m (C.MStack VB.Vector (PrimState m) Int)
se = C.replicate 10 0

ss :: PrimMonad m => m (C.MStack VB.Vector (PrimState m) Int)
ss = se >>= C.push 13

s3 :: PrimMonad m => m (C.MStack VB.Vector (PrimState m) Int)
s3 = ss >>= C.push 12 >>= C.push 11

s3' :: PrimMonad m => m (C.MStack VB.Vector (PrimState m) Int)
s3' = se >>= (fmap snd <$> C.pop) >>= C.push 1 >>= C.push 2 >>= C.push 3

fromTo :: VB.Vector Int -> VB.Vector Int
fromTo v = runST $ C.fromVector v >>= C.toVector

prop_from_to_id :: VB.Vector Int -> Bool
prop_from_to_id v
  | VB.length v == 0 = True
  | otherwise = fromTo v == v

prop_push_get :: Int -> VB.Vector Int -> Bool
prop_push_get x v
  | VB.length v == 0 = True
  | otherwise = x == runST (C.fromVector v >>= C.push x >>= C.get)

prop_push :: Int -> VB.Vector Int -> Bool
prop_push x v
  | VB.length v == 0 = True
  | otherwise =
      runST (C.fromVector v >>= C.push x >>= C.toVector)
        == VB.tail v VB.++ VB.singleton x

prop_many_pushes :: [Int] -> VB.Vector Int -> Bool
prop_many_pushes xs v
  | VB.length v == 0 = True
  | length xs <= VB.length v = True
  | otherwise =
      runST
        ( do
            ms <- C.fromVector v
            ms' <- foldM (flip C.push) ms xs
            C.toVector ms'
        )
        == sol
  where
    nl = length xs
    nv = VB.length v
    -- This was hard :).
    sol = VB.drop nl v VB.++ VB.fromList (reverse $ take nv $ reverse xs)

prop_json :: C.Stack VB.Vector Int -> Bool
prop_json c = Success c == fromJSON (toJSON c)

-- We initialize a stack, push some values and take some values.
prop_push_take :: Int -> [Int] -> VB.Vector Int -> Bool
prop_push_take k l v
  | VB.length v == 0 = True
  | otherwise =
      -- traceShow
      --   ( "Input:" ++ show k ++ " " ++ show l ++ " " ++ show v
      --       ++ " k': "
      --       ++ show k'
      --       ++ " Stack full: "
      --       ++ show stackFull
      --       ++ " Stack take: "
      --       ++ show stackTake
      --       ++ " Sol: "
      --       ++ show solution
      --   ) $
      stackTake == solution
  where
    -- stackFull = runST $ do
    --   m <- C.fromVector v
    --   m' <- foldM (flip C.push) m l
    --   C.toVector m'
    stackTake = runST $ do
      m <- C.fromVector v
      m' <- foldM (flip C.push) m l
      C.take k' m'
    nl = length l
    nv = VB.length v
    -- We have to take a non-negative number of elements, and cannot take more
    -- elements than the stack size.
    k' = min (abs k) nv
    -- Also this was hard :).
    solution =
      if k' <= nl
        then VB.fromList (reverse $ take k' $ reverse l)
        else -- k' is larger than nl but lower than nv.
        --
        -- We take all the nl elements from the list and (k' - nl) elements from
        -- the end of the vector.
        --
        -- So we need to drop (nv - k' + nl) from the beginning of the vector.
          VB.drop (nv - k' + nl) v VB.++ VB.fromList l

-- We initialize a stack, push some values and take some values.
prop_fold_independent_of_index :: VB.Vector Int -> Bool
prop_fold_independent_of_index v
  | VB.length v == 0 = True
  | otherwise = do
      [solV] == nub solSs
  where
    n = VB.length v
    solV = VB.sum v
    solSs =
      runST $
        sequence
          [C.fromVectorWithIndex i v >>= C.foldM (+) 0 | i <- [0 .. n - 1 :: Int]]

spec :: Spec
spec = do
  describe "construction" $
    it "doesn't choke on weird inputs" $ do
      runST (se >>= C.toVector) `shouldBe` VB.replicate 10 0
      runST (ss >>= C.get) `shouldBe` 13

  describe "conversion identities" $ do
    it "correctly converts partly filled stacks" $
      runST (ss >>= C.toVector) `shouldBe` (VB.replicate 9 0 `VB.snoc` 13)
    prop "toVector . fromVector is identity" (prop_from_to_id :: VB.Vector Int -> Bool)

  describe "conversion failure" $
    it "fails to convert empty vectors" $
      evaluate (runST $ C.fromVector VB.empty >>= C.freeze) `shouldThrow` anyErrorCall

  describe "foldKM over end" $
    it "works" $ do
      runST (s3 >>= C.foldKM 1 (+) 0) `shouldBe` 11
      runST (s3 >>= C.foldKM 2 (+) 0) `shouldBe` (11 + 12)
      runST (s3 >>= C.foldKM 3 (+) 0) `shouldBe` (11 + 12 + 13)
      runST (s3 >>= C.foldKM 4 (+) 0) `shouldBe` (11 + 12 + 13)
      runST (s3 >>= C.foldKM 3 (*) 1) `shouldBe` (11 * 12 * 13)
      runST (s3 >>= C.foldKM 4 (*) 1) `shouldBe` 0
      runST (s3' >>= C.foldKM 3 (*) 1) `shouldBe` 6
      runST (s3' >>= C.foldKM 4 (*) 1) `shouldBe` 0

  describe "properties" $ do
    prop "push" prop_push
    prop "push_get" prop_push_get
    prop "many pushes" prop_many_pushes
    prop "json" prop_json
    prop "push_take" prop_push_take
    prop "fold_independent_of_index" prop_fold_independent_of_index
