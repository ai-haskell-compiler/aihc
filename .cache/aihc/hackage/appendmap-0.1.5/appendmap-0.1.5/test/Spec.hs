{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map.Append.Lazy   as Lazy
import qualified Data.Map.Lazy          as Lazy

import qualified Data.Map.Append.Strict as Strict
import qualified Data.Map.Strict        as Strict

import qualified Data.Map.Counter       as Counter

import           Data.Maybe

import           Data.Semigroup

import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Test.Hspec
import           Test.QuickCheck

data AppendMap appendmap map = AppendMap
  { mapFromList :: !(forall v. [(Int, v)] -> map Int v)
  , mapKeys     :: !(forall v. map Int v -> Set Int)
  , mapLookup   :: !(forall v. Int -> map Int v -> Maybe v)
  , mapEmpty    :: !(map Int String)
  , appendMap   :: !(forall k v. map k v -> appendmap k v)
  , unAppendMap :: !(forall k v. appendmap k v -> map k v)
  }

lazy :: AppendMap Lazy.AppendMap Lazy.Map
lazy =
  AppendMap
    { mapFromList = Lazy.fromList
    , mapKeys = Lazy.keysSet
    , mapLookup = Lazy.lookup
    , mapEmpty = Lazy.empty
    , appendMap = Lazy.AppendMap
    , unAppendMap = Lazy.unAppendMap
    }

strict :: AppendMap Strict.AppendMap Strict.Map
strict =
  AppendMap
    { mapFromList = Strict.fromList
    , mapKeys = Strict.keysSet
    , mapLookup = Strict.lookup
    , mapEmpty = Strict.empty
    , appendMap = Strict.AppendMap
    , unAppendMap = Strict.unAppendMap
    }

appendMapSpec ::
     ( Show (map Int String)
     , Eq (map Int String)
     , Arbitrary (map Int String)
     , Semigroup (appendmap Int String)
     , Monoid (appendmap Int String)
     )
  => AppendMap appendmap map
  -> Spec
appendMapSpec AppendMap {..} = do
  it "has empty element" $ unAppendMap mempty `shouldBe` mapEmpty
  it "combines elements" $ do
    let one = appendMap $ mapFromList [(1, "hello"), (2, "goodbye")]
    let two = appendMap $ mapFromList [(1, "world"), (3, "again")]
    unAppendMap (one <> two) `shouldBe`
      mapFromList [(1, "helloworld"), (2, "goodbye"), (3, "again")]
  it "is distributive against mconcat" $
    property $ \(maps :: [map Int String]) ->
      let appended = unAppendMap $ mconcat $ map appendMap maps
          individuallyAppended =
            mapFromList
              [ (k, mconcat $ mapMaybe (mapLookup k) maps)
              | k <- Set.toList $ Set.unions $ map mapKeys maps
              ]
       in appended === individuallyAppended
  it "is distributive over lookup" $
    property $ \(m1' :: map Int String) (m2' :: map Int String) (k :: Int) ->
      let lookup key = mapLookup key . unAppendMap
          m1 = appendMap m1'
          m2 = appendMap m2'
       in lookup k (m1 <> m2) === lookup k m1 <> lookup k m2

counterSpec :: Spec
counterSpec =
  it "counts entries" $ do
    let counted =
          Counter.mkCounter "one" <> Counter.mkCounter "two" <>
          Counter.mkCounter "two"
    let result = Counter.getCounts counted
    Strict.lookup "one" result `shouldBe` Just 1
    Strict.lookup "two" result `shouldBe` Just 2

main :: IO ()
main =
  hspec $ do
    describe "Data.Map.Append.Lazy" $ appendMapSpec lazy
    describe "Data.Map.Append.Strict" $ appendMapSpec strict
    describe "Data.Map.Counter" counterSpec
