{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.DEPQ as D
import Data.DEPQ (DEPQ)
import Test.QuickCheck (Arbitrary(..), Property, elements, property, (===))
import Test.Hspec (Spec, describe, hspec, it)
import Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Foldable as F (Foldable(..))

main :: IO ()
main = hspec $ modifyMaxSuccess (const 1000) $ do
    validitySpec
    postConditionSpec
    metamorphicSpec
    modelBasedSpec

--------------------------------------------------------------------------------
-- Setting everything up
--------------------------------------------------------------------------------

-- We want our QuickCheck generators to generate edge cases which are:

-- 1. When we try to insert a new value into DEPQ
-- 2. When modification function tries to overwrite existing values
-- 3. When DEPQ contains multiple key-pair values in which the key is different 
-- (e.g insert 4 A "Val" $ insert 5 A "Val2" empty)

-- To enable this, we use Priority type in which the range of values
-- are somewhat small and readable.
data Priority
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  deriving (Eq, Enum, Ord, Show)

instance Arbitrary Priority where
    arbitrary = elements [A, B, C, D, E, F, G, H]

type Val = Int

type TestDEPQ = DEPQ Priority Val

--------------------------------------------------------------------------------
-- Validity testing
-- These sets of tests prove that given function will always return valid DEPQ
--------------------------------------------------------------------------------

validitySpec :: Spec
validitySpec = describe "Validity test" $ do
    it "null" empty_is_null
    prop "Generated DEPQ always valid" generator_valid
    prop "insert" insert_valid
    prop "fromList" fromList_valid
    prop "deleteMin" deleteMin_valid
    prop "deleteMax" deleteMax_valid
    prop "popMin" popMin_valid
    prop "popMax" popMax_valid

-- Test that DEPQ generated from QuickCheck are always valid
generator_valid :: TestDEPQ -> Property
generator_valid = property . D.valid

insert_valid :: Int -> Priority -> Val -> TestDEPQ -> Property
insert_valid key priority value  depq = 
  property $ D.valid $ D.insert key priority value depq 

fromList_valid :: [(Int, Priority, Val)] -> Property
fromList_valid = property . D.valid . D.fromList

deleteMin_valid :: TestDEPQ -> Property
deleteMin_valid = property . D.valid . D.deleteMin

deleteMax_valid :: TestDEPQ -> Property
deleteMax_valid = property . D.valid . D.deleteMax

popMin_valid :: TestDEPQ -> Property
popMin_valid depq = 
    property $ maybe True (\(_val, depq') -> D.valid depq') (D.popMin depq)

popMax_valid :: TestDEPQ -> Property
popMax_valid depq =
    property $ maybe True (\(_val, depq') -> D.valid depq') (D.popMin depq)

empty_is_null :: Bool
empty_is_null = D.null D.empty

--------------------------------------------------------------------------------
-- Post condition
-- Here, we test that given function returns expected value
--------------------------------------------------------------------------------

postConditionSpec :: Spec
postConditionSpec = describe "Post condition" $ do
    prop "findMin" findMin_post_condition
    prop "findMax" findMax_post_condition
    prop "size" size_post_condition
    prop "topK" topK_post_condition
    prop "bottomK" bottomK_post_condition

findMin_post_condition :: Int -> Priority -> Val -> Property
findMin_post_condition priority key val =
    let depq = D.insert priority key val D.empty
    in D.findMin depq === Just (priority, key, val)

findMax_post_condition :: Int -> Priority -> Val -> Property
findMax_post_condition priority key val =
    let depq = D.insert priority key val D.empty
    in D.findMax depq === Just (priority, key, val)

size_post_condition :: Int -> Priority -> Val -> TestDEPQ -> Property
size_post_condition key priority val depq =
    property $ D.size (D.insert key priority val depq) >= D.size depq

topK_post_condition :: Int -> TestDEPQ -> Property
topK_post_condition num depq =
    let s = D.topK num depq
    in if num <= (D.size depq) && num > 0
        then Seq.length s === num
        else Seq.length s === 0

bottomK_post_condition :: Int -> TestDEPQ -> Property
bottomK_post_condition num depq =
    let s = D.bottomK num depq
    in if num <= (D.size depq) && num > 0
        then Seq.length s === num
        else Seq.length s === 0

popMin_post_condition :: TestDEPQ -> Property
popMin_post_condition depq = 
    let mPoped = D.popMin depq
    in if D.size depq == 0
        then mPoped === Nothing
        else property $ maybe
          False
          (\(_val, depq') -> D.size depq' < D.size depq)
          mPoped

popMax_post_condition :: TestDEPQ -> Property
popMax_post_condition depq = 
    let mPoped = D.popMax depq
    in if D.size depq == 0
        then mPoped === Nothing
        else property $ maybe
          False
          (\(_val, depq') -> D.size depq' < D.size depq)
          mPoped

--------------------------------------------------------------------------------
-- Metamorphic
-- Instead of testing single function, we're now going to test multiple set of
-- functions
--------------------------------------------------------------------------------

metamorphicSpec :: Spec
metamorphicSpec = describe "Metamorphic" $ do
    prop "insert twice, findMax" insert_insert_findMax
    prop "insert twice, findMin" insert_insert_findMin

-- Insert 2 key value pairs, test that findMax return expected value
insert_insert_findMax :: (Int, Priority, Val) -> (Int, Priority, Val) -> Property
insert_insert_findMax t1@(k1, p1, v1) t2@(k2, p2, v2)
      | k1 == k2 = D.findMax depq === Just t2
      | p1 < p2 = D.findMax depq === Just t2
      | p1 > p2 = D.findMax depq === Just t1
      | k1 > k2 = D.findMax depq === Just t2
      | otherwise = D.findMax depq === Just t1
  where
    depq = D.insert k2 p2 v2 $ D.insert k1 p1 v1 D.empty

-- Insert 2 key value pairs, test that findMin return expected value
insert_insert_findMin :: (Int, Priority, Val) -> (Int, Priority, Val) -> Property
insert_insert_findMin t1@(k1, p1, v1) t2@(k2, p2, v2)
      | k1 == k2 = D.findMin depq === Just t2
      | p1 < p2 = D.findMin depq === Just t1
      | p1 > p2 = D.findMin depq === Just t2
      | k1 > k2 = D.findMin depq === Just t2
      | otherwise = D.findMin depq === Just t1
  where
    depq = D.insert k2 p2 v2 $ D.insert k1 p1 v1 D.empty

--------------------------------------------------------------------------------
-- Model based testing
-- Here, we test against a data structure that would act as an model
-- (i.e. Something that would behave similar to DEPQ and is well tested)
--------------------------------------------------------------------------------

{-|
     --------  fromDEPQ ---------
    |  DEPQ | ------ > |  Model |
    --------           ---------
        |                  |
       ｜　f(DEPQ)　  　　　 | g(Model)
       |　　　　　　　　　　　|
     -------------------------
    |   f(DEPQ) ==  g(Model) |
     ------------------------
-}

-- | Model of 'DEPQ'
type Model = Map Int (Priority, Val)

-- | /O(n)/ Convert a queue to a Sequence of (key, priority, value) tuples. 
toList :: (Ord p, Ord a) => DEPQ p a -> [(Int, p, a)]
toList depq = L.sort $ F.toList $ D.topK (D.size depq) depq

fromDEPQ :: TestDEPQ -> Model
fromDEPQ depq = L.foldl' f mempty (toList depq)
  where
    f :: Model -> (Int, Priority, Val) -> Model
    f accum (key, priority, val) = M.insert key (priority, val) accum

modelToList :: Model -> [(Int, Priority, Val)]
modelToList model = L.sort $ map convert $ M.toList model

-- Helful functions

-- This is helpful when converting
convert :: (Int, (Priority, Val)) -> (Int, Priority, Val)
convert (key, (priority, val)) = (key, priority, val)

-- Get minimal value of an given 'Priority'
getVal :: Priority -> Model -> Maybe (Int, (Priority, Val))
getVal priority model = M.lookupMin $ M.filter (\(p, _) -> p == priority) model

getPriority :: (a, Priority, b) -> Priority
getPriority (_, p, _) = p

-- Set of functions which should behave the same way as the one that we're testing
-- against

size :: Model -> Int
size model = length $ map convert $ M.toList model

insert :: Int -> Priority -> Val -> Model -> Model
insert key priority val model = M.insert key (priority, val) model

lookupMax :: Model -> Maybe (Int, Priority, Val)
lookupMax model = 
    if M.null model
        then Nothing
        else 
          let (_, maxPriority, _) = L.maximumBy
                (\t1 t2 -> getPriority t1 `compare` getPriority t2) $ modelToList model
          in convert <$> getVal maxPriority model

lookupMin :: Model -> Maybe (Int, Priority, Val)
lookupMin model =
    if M.null model
        then Nothing
        else
          let (_, minPriority, _) = L.minimumBy
                (\t1 t2 -> getPriority t1 `compare` getPriority t2) (modelToList model)
          in convert <$> getVal minPriority model

takeTop :: Int -> Model -> Seq (Int, Priority, Val)
takeTop num model =
    let sortedList = L.sortBy 
          (\t1 t2 -> getPriority t2 `compare` getPriority t1) $ modelToList model
    in Seq.fromList $ take num sortedList

takeBottom :: Int -> Model -> Seq (Int, Priority, Val)
takeBottom num model =
    let sortedList =
          L.sortBy (\t1 t2 -> getPriority t1 `compare` getPriority t2) $ modelToList model
    in Seq.fromList $ take num sortedList

deleteMax :: Model -> Model
deleteMax model =
    if M.null model
        then model
        -- It's safe to use partial functions since we know that the model has
        -- at least 1 element
        else
            let (_, maxPriority, _) = L.maximumBy
                  (\t1 t2 -> getPriority t1 `compare` getPriority t2)
                  (modelToList model)
                (k, _) = fromJust $ getVal maxPriority model
            in M.delete k model

deleteMin :: Model -> Model
deleteMin model =
    if M.null model
        then model
        else
            let (_, minPriority, _) = L.minimumBy
                  (\t1 t2 -> getPriority t1 `compare` getPriority t2) $ modelToList model
                (k, _) = fromJust $ getVal minPriority model
            in M.delete k model

-- Tests

modelBasedSpec :: Spec
modelBasedSpec = describe "Model based testing" $ do
    it "empty" nil_model
    prop "size" size_model
    prop "findMax" findMax_model
    prop "findMin" findMin_model
    prop "bottomK" bottomK_model
    prop "topK" topK_model
    prop "deleteMax" deleteMax_model
    prop "deleteMin" deleteMin_model

-- Tests

nil_model :: Property
nil_model = (fromDEPQ D.empty) === (mempty :: Model)

size_model :: TestDEPQ -> Property
size_model depq = D.size depq === size (fromDEPQ depq)

findMax_model :: TestDEPQ -> Property
findMax_model depq = D.findMax depq === lookupMax (fromDEPQ depq)

findMin_model :: TestDEPQ -> Property
findMin_model depq = D.findMin depq === lookupMin (fromDEPQ depq)

bottomK_model :: Int -> TestDEPQ -> Property
bottomK_model num depq = 
    let s = D.bottomK num depq
    in if num <= D.size depq
        then s === takeBottom num (fromDEPQ depq)
        else s === mempty

topK_model :: Int -> TestDEPQ -> Property
topK_model num depq =
    let s = D.topK num depq
    in if num <= D.size depq
        then s === takeTop num (fromDEPQ depq)
        else s === mempty

-- Here, we check that the Model and DEPQ contains exact same content after
-- applying an function
hasContent :: TestDEPQ -> Model -> Property
hasContent depq model = toList depq === (modelToList model)

insert_model :: Int -> Priority -> Val -> TestDEPQ -> Property
insert_model key priority val depq = 
    D.insert key priority val depq `hasContent` insert key priority val (fromDEPQ depq)

deleteMax_model :: TestDEPQ -> Property
deleteMax_model depq = D.deleteMax depq `hasContent` (deleteMax $ fromDEPQ depq)

deleteMin_model :: TestDEPQ -> Property
deleteMin_model depq = D.deleteMin depq `hasContent` (deleteMin $ fromDEPQ depq)
