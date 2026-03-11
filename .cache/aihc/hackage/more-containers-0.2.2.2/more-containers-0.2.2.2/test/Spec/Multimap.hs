{-# LANGUAGE OverloadedLists #-}

module Spec.Multimap (
  listMultimapSpec,
  seqMultimapSpec,
  setMultimapSpec
) where

import Data.Binary (decode, encode)
import Data.Char (toUpper)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Multimap
import Data.Multimap.List
import Data.Multimap.Seq
import qualified Data.Multimap.Set as SetMmap

listMultimapSpec :: Spec
listMultimapSpec = describe "ListMultimap" $ do
  it "should cons" $ do
    let m = fromGroupList [(1, "abc"), (2, "")]
    cons 3 'a' m `shouldBe` fromGroupList [(1, "abc"), (3, "a")]
  it "should uncons" $ do
    let m = fromGroupList [(1, "aa")]
    uncons 1 m `shouldBe` Just ('a', singleton 1 'a')
    uncons 2 m `shouldBe` Nothing
  it "can be traversed" $ do
    let
      m1 = [('a', Just 1), ('a', Just 2)] :: ListMultimap Char (Maybe Int)
      m2 = [('b', Nothing)] :: ListMultimap Char (Maybe Int)
      m3 = fromMap $ Map.singleton 'a' [1, 2]
    sequenceA m1 `shouldBe` Just m3
    sequenceA (m1 <> m2) `shouldBe` Nothing
  it "converts to set multimap" $ do
    let
      m1 = fromList [('a', 1), ('a', 1)] :: ListMultimap Char Int
      m2 = mapGroups (fmap Set.fromList) m1 :: SetMultimap Char Int
    m2 `shouldBe` fromList [('a', 1)]
  it "supports min and max views" $ do
    let m = fromGroupList [(1, "ab"), (2, "c")]
    minViewWith List.uncons m `shouldBe` Just ((1, 'a'), fromGroupList [(1, "b"), (2, "c")])
    maxViewWith List.uncons m `shouldBe` Just ((2, 'c'), fromGroupList [(1, "ab")])
    maxViewWith List.uncons (empty :: ListMultimap Int Char) `shouldBe` Nothing

seqMultimapSpec :: Spec
seqMultimapSpec = describe "SeqMultimap" $ do
  it "should pop" $ do
    let m = fromList [(1, 'a'), (1, 'b'), (2, 'c')] :: SeqMultimap Int Char
    popFirst 1 m `shouldBe` Just ('a', fromList [(1, 'b'), (2, 'c')])
    popLast 1 m `shouldBe` Just ('b', fromList [(1, 'a'), (2, 'c')])
    popFirst 2 m `shouldBe` Just ('c', fromList [(1, 'a'), (1, 'b')])
    popLast 3 m `shouldBe` Nothing

setMultimapSpec :: Spec
setMultimapSpec = describe "SetMultimap" $ do
  it "should map" $ do
    let m = fromList [(1, 'a'), (1, 'A')] :: SetMultimap Int Char
    size m `shouldBe` 2
    distinctSize m `shouldBe` 1
    SetMmap.map toUpper m `shouldBe` singleton 1 'A'
  it "should delete" $ do
    let m = singleton 2 'a'
    SetMmap.delete 1 'a' empty `shouldBe` empty
    SetMmap.delete 2 'a' m `shouldBe` empty
    SetMmap.delete 2 'b' m `shouldBe` m
    SetMmap.delete 1 'a' m `shouldBe` m
  it "should check membership" $ do
    let m = fromList [(1, 'a'), (2, 'b')] :: SetMultimap Int Char
    SetMmap.member' 1 'a' m `shouldBe` True
    SetMmap.member' 1 'b' m `shouldBe` False
    SetMmap.notMember' 1 'b' m `shouldBe` True
  it "can be serialized" $ do
    let
      m = fromList [(1, 'a'), (2, 'b')] :: SetMultimap Int Char
      bs = encode m
    decode bs `shouldBe` m
