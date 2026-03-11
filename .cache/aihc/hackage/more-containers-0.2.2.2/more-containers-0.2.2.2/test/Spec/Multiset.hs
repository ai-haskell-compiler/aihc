{-# LANGUAGE OverloadedLists #-}

module Spec.Multiset (
  multisetSpec
) where

import Prelude hiding (max, min, null, replicate)

import Data.Binary (decode, encode)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Multiset

multisetSpec :: Spec
multisetSpec = describe "Multiset" $ do
  it "should create empty instances" $ do
    size empty `shouldBe` 0
    distinctSize empty `shouldBe` 0
    null empty `shouldBe` True
    count 'a' empty `shouldBe` 0
  it "should create singletons" $ do
    size (singleton 'a') `shouldBe` 1
    null (singleton 'b') `shouldBe` False
    distinctSize (singleton 'c') `shouldBe` 1
    member 'c' (singleton 'b') `shouldBe` False
    count 'c' (singleton 'c') `shouldBe` 1
  it "should create valid replicates" $ do
    size (replicate (-1) 'a') `shouldBe` 0
    size (replicate 0 'a') `shouldBe` 0
    size (replicate 1 'b') `shouldBe` 1
    size (replicate 2 'c') `shouldBe` 2
    distinctSize (replicate 2 'd') `shouldBe` 1
    member 'c' (replicate 2 'c') `shouldBe` True
    notMember 'c' (replicate 1 'd') `shouldBe` True
  it "should deserialize lists" $ do
    let ms = fromGroupList [('a', 2), ('b', 1)]
    ms `shouldBe` fromList "aba"
    count 'a' ms `shouldBe` 2
    fromList [] `shouldBe` (empty :: Multiset Char)
  it "should deserialize maps" $ do
    fromCountMap Map.empty `shouldBe` (empty :: Multiset Char)
    fromCountMap (Map.singleton 'a' 23) `shouldBe` replicate 23 'a'
  it "support subset checks" $ do
    let
      m1 = fromList "abbbcd"
      m2 = fromList "abbc"
    isSubsetOf m2 m1 `shouldBe` True
    isSubsetOf m1 m1 `shouldBe` True
    isProperSubsetOf m2 m1 `shouldBe` True
    isProperSubsetOf m1 m1 `shouldBe` False
  it "returns the most common" $ do
    mostCommon (fromList "abbbcda") `shouldBe` [(3, "b"), (2, "a"), (1, "cd")]
  it "can insert and remove elements" $ do
    insert 'a' empty `shouldBe` singleton 'a'
    insert 'a' (singleton 'b') `shouldBe` fromList "ab"
    remove 'a' empty `shouldBe` empty
    remove 'a' (singleton 'b') `shouldBe` singleton 'b'
    removeAll 'a' (singleton 'b') `shouldBe` singleton 'b'
    removeAll 'a' (replicate 3 'a') `shouldBe` empty
  it "can be folded" $ do
    foldl' (+) 0 (singleton 3) `shouldBe` 3
    foldl' (+) 0 (fromList [1, 2, 4]) `shouldBe` 7
  it "can be modified" $ do
    modify (*3) 'a' empty `shouldBe` empty
    modify (*2) 'a' (singleton 'a') `shouldBe` replicate 2 'a'
    modify (*4) 'a' (singleton 'b') `shouldBe` singleton 'b'
  it "can be count mapped" $ do
    mapCounts (*3) empty `shouldBe` (empty :: Multiset Char)
    mapCounts (*2) (singleton 'a') `shouldBe` replicate 2 'a'
    mapCounts (*0) (singleton 'a') `shouldBe` empty
  it "can be combined" $ do
    fromList "abb" <> fromList "aabc" `shouldBe` fromList "aaabbbc"
    fromList "abb" `max` fromList "aab" `shouldBe` fromList "aabb"
    fromList "abb" `min` fromList "aab" `shouldBe` fromList "ab"
    fromList "abb" `difference` fromList "aab" `shouldBe` fromList "b"
  it "returns elements" $ do
    elems (fromList "") `shouldBe` ""
    elems (fromList "aaa") `shouldBe` "aaa"
  it "can be serialized" $ do
    let
      ms = fromList "aabc"
      bs = encode ms
    decode bs `shouldBe` ms
  it "supports min and max views" $ do
    let ms = fromList "aabc"
    maxView ms `shouldBe` Just ('c', fromList "aab")
    minView ms `shouldBe` Just ('a', fromList "abc")
    minView (empty :: Multiset Char) `shouldBe` Nothing
