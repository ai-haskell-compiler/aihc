{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Test.Tasty.Bench

import qualified Data.Trie.Set as TSet
import Data.Trie.Map (TMap)
import qualified Data.Trie.Map as TMap

import Data.Monoid
import Data.List (inits, tails, foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Common

main :: IO ()
main = defaultMain
  [ benchAll englishDataset "English"
  , benchAll wikiDataset "Wiki"
  ]

benchAll :: IO Dataset -> String -> Benchmark
benchAll setupEnv groupName =
  env setupEnv $ \dataset ->
    bgroup groupName [
      benchSet dataset,
      benchTSet dataset,
      benchMap dataset,
      benchTMap dataset
      ]

benchTSet :: Dataset -> Benchmark
benchTSet ~Dataset{..} = bgroup "TSet" 
  [ bgroup "construction"
      [ bench "fromList" $ whnf TSet.fromList dictAUnsorted
      , bench "fromAscList" $ whnf TSet.fromAscList dictA ]
  , env (pure $ TSet.fromList dictA) $ \mapA ->
      bgroup "query"
        [ bench "isEmpty" (nf TSet.null mapA)
        , bench "stringCount" (nf TSet.count mapA)
        , bench "enumerate10" (nf (take 10 . TSet.enumerate) mapA)
        , bench "enumerateAll" (nf TSet.enumerate mapA)
        , bench "member" (nf (\dict' -> map (`TSet.member` dict') gibberish) mapA)
        , bench "beginWith" (whnf (`TSet.beginWith` shortKey) mapA) ]
  , env (pure (TSet.fromList dictA)) $ \mapA ->
      bgroup "single-item"
        [ bench "insert1" (whnf (TSet.insert "######fake_key######") mapA)
        , bench "insert2" (whnf (TSet.insert realKey) mapA)
        , bench "delete1" (whnf (TSet.delete "######fake_key######") mapA)
        , bench "delete2" (whnf (TSet.delete realKey) mapA)
        ]
  , env (pure $ TSet.fromList dictA) $ \mapA ->
    env (pure $ TSet.fromList dictB) $ \mapB ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry TSet.union) (mapA, mapB))
        , bench "intersection" (whnf (uncurry TSet.intersection) (mapA, mapB))
        , bench "difference" (whnf (uncurry TSet.difference) (mapA, mapB))
        , env (pure $ TSet.fromList gibberish) $ \mapSmall ->
            bench "append" (whnf (uncurry TSet.append) (mapSmall, mapSmall))
        , bench "prefixes" (whnf TSet.prefixes mapA)
        , bench "suffixes" (whnf TSet.suffixes mapB) ]
  ]
  where
    realKey = head dictA
    shortKey = take 3 realKey

benchSet :: Dataset -> Benchmark
benchSet ~Dataset{..} = bgroup "Set"
  [ bgroup "construction"
      -- Set.fromList detects whether the input list is sorted
      -- and switch the algorithm based on it.
      -- Using shuffled dictionary avoids this optimization fires
      -- in this benchmark.
      [ bench "fromList" $ whnf Set.fromList dictAUnsorted
      , bench "fromAscList" $ whnf Set.fromAscList dictA ]
  , env (pure $ Set.fromList dictA) $ \mapA ->
      bgroup "query"
        [ bench "isEmpty" (nf Set.null mapA)
        , bench "stringCount" (nf Set.size mapA)
        , bench "enumerate10" (nf (take 10 . Set.toList) mapA)
        , bench "enumerateAll" (nf Set.toList mapA)
        , bench "member" (nf (\dict' -> map (`Set.member` dict') gibberish) mapA)
        , bench "beginWith" (whnf (`setBeginWith` shortKey) mapA) ]
  , env (pure (Set.fromList dictA)) $ \mapA ->
      bgroup "single-item"
        [ bench "insert1" (whnf (Set.insert "######fake_key######") mapA)
        , bench "insert2" (whnf (Set.insert realKey) mapA)
        , bench "delete1" (whnf (Set.delete "######fake_key######") mapA)
        , bench "delete2" (whnf (Set.delete realKey) mapA)
        ]
  , env (pure $ Set.fromList dictA) $ \mapA ->
    env (pure $ Set.fromList dictB) $ \mapB ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry Set.union) (mapA, mapB))
        , bench "intersection" (whnf (uncurry Set.intersection) (mapA, mapB))
        , bench "difference" (whnf (uncurry Set.difference) (mapA, mapB))
        , env (pure $ Set.fromList gibberish) $ \mapSmall ->
            bench "append" (whnf (uncurry setAppend) (mapSmall, mapSmall))
        , bench "prefixes" (whnf setPrefixes mapA)
        , bench "suffixes" (whnf setSuffixes mapB) ]
  ]
  where
    realKey = head dictA
    shortKey = take 3 realKey

setAppend :: (Ord c) => Set [c] -> Set [c] -> Set [c]
setAppend ass bss = Set.unions
  [ Set.mapMonotonic (as ++) bss
      | as <- Set.toAscList ass ]

setPrefixes :: (Ord c) => Set [c] -> Set [c]
setPrefixes ass = Set.unions
  [ Set.fromDistinctAscList (inits as) | as <- Set.toAscList ass ]

setSuffixes :: (Ord c) => Set [c] -> Set [c]
setSuffixes ass = Set.fromList
  [ bs | as <- Set.toAscList ass, bs <- tails as ]

setBeginWith :: (Ord c) => Set [c] -> [c] -> Set [c]
setBeginWith ass prefix =
  let n = length prefix
      -- ass' = { as | as ∈ ass, as >= prefix }
      ass' = Set.dropWhileAntitone (< prefix) ass
      -- ass'' = { as | as ∈ ass', prefix `isPrefixOf` as }
      -- Note: `isPrefix prefix :: [c] -> Bool` is antitone predicate for ass'!
      --       In fact, take any `xs, ys` such that `prefix <= xs <= ys`.
      --       Then `isPrefix prefix ys ==> isPrefix prefix xs` holds.
      ass'' = Set.takeWhileAntitone (isPrefixOf prefix) ass'
  in Set.mapMonotonic (drop n) ass''

isPrefixOf :: Eq c => [c] -> [c] -> Bool
isPrefixOf [] _ = True
isPrefixOf (_:_) [] = False
isPrefixOf (p:ps) (a:as) = p == a && isPrefixOf ps as

benchTMap :: Dataset -> Benchmark
benchTMap ~Dataset{..} = bgroup "TMap" 
  [ bgroup "construction"
      [ bench "fromList" $ whnf TMap.fromList [(w, length w) | w <- dictAUnsorted ]
      , bench "fromAscList" $ whnf TMap.fromAscList [(w, length w) | w <- dictA ]
      ]
  , env (pure $ lenTMap dictA) $ \mapA ->
      bgroup "query"
        [ bench "isEmpty" (nf TMap.null mapA)
        , bench "stringCount" (nf TMap.count mapA)
        , bench "enumerate10" (nf (take 10 . TMap.toList) mapA)
        , bench "lookupPrefixes" $ nf (TMap.lookupPrefixes longKey) mapA
        , bench "member" (nf (\dict' -> map (`TMap.member` dict') gibberish) mapA)
        , bench "match" (whnf (consumeMatch . TMap.match shortKey) mapA) ]
  , env (pure (lenTMap dictA)) $ \mapA ->
      bgroup "single-item"
        [ bench "insert1" (whnf (TMap.insert "######fake_key######" 1) mapA)
        , bench "insert2" (whnf (TMap.insert realKey 1) mapA)
        , bench "delete1" (whnf (TMap.delete "######fake_key######") mapA)
        , bench "delete2" (whnf (TMap.delete realKey) mapA)
        , bench "alter1" (whnf (TMap.alter alterFn "######fake_key######") mapA)
        , bench "alter2" (whnf (TMap.alter alterFn realKey) mapA)
        ]
  , env (pure (lenTMap dictA)) $ \mapA ->
      bgroup "traversal"
        [ bench "fmap" (nf (fmap (+3)) mapA)
        , bench "foldMap" (nf (foldMap Sum) mapA) ]
  , env (pure $ lenTMap dictA) $ \mapA ->
    env (pure $ lenTMap dictB) $ \mapB ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry TMap.union) (mapA, mapB))
        , bench "intersection" (whnf (uncurry TMap.intersection) (mapA, mapB))
        , bench "difference" (whnf (uncurry TMap.difference) (mapA, mapB))
        , env (pure $ lenTMap gibberish) $ \mapSmall ->
            bench "append" (whnf (uncurry tmapProd) (mapSmall, mapSmall)) ]
  ]
  where
    realKey = head dictA
    longKey = concat (replicate 100 realKey)
    shortKey = take 3 realKey

alterFn :: Maybe Int -> Maybe Int
alterFn Nothing = Nothing
alterFn (Just a) = if even a then Just a else Nothing

lenTMap :: (Ord c) => [[c]] -> TMap c Int
lenTMap dict = TMap.fromList [(w, length w) | w <- dict]

tmapProd :: (Ord c) => TMap c Int -> TMap c Int -> TMap c (Sum Int)
tmapProd = TMap.appendWith (\x y -> Sum (x * y))

consumeMatch :: (Maybe a, r) -> r
consumeMatch (ma, r) = (maybe () (`seq` ()) ma) `seq` r
{-# INLINE consumeMatch #-}

benchMap :: Dataset -> Benchmark
benchMap ~Dataset{..} = bgroup "Map" 
  [ bgroup "construction"
      [ bench "fromList" $ whnf Map.fromList [(w, length w) | w <- dictAUnsorted ]
      , bench "fromAscList" $ whnf Map.fromAscList [(w, length w) | w <- dictA ]
      ]
  , env (pure $ lenMap dictA) $ \mapA ->
      bgroup "query"
        [ bench "isEmpty" (nf Map.null mapA)
        , bench "stringCount" (nf Map.size mapA)
        , bench "enumerate10" (nf (take 10 . Map.toList) mapA)
        , bench "lookupPrefixes" $ nf (mapLookupPrefixes longKey) mapA
        , bench "member" (nf (\dict' -> map (`Map.member` dict') gibberish) mapA)
        , bench "match" (whnf (consumeMatch . mapMatch shortKey) mapA)
        ]
  , env (pure (lenMap dictA)) $ \mapA ->
      bgroup "single-item"
        [ bench "insert1" (whnf (Map.insert "######fake_key######" 1) mapA)
        , bench "insert2" (whnf (Map.insert realKey 1) mapA)
        , bench "delete1" (whnf (Map.delete "######fake_key######") mapA)
        , bench "delete2" (whnf (Map.delete realKey) mapA)
        , bench "alter1" (whnf (Map.alter alterFn "######fake_key######") mapA)
        , bench "alter2" (whnf (Map.alter alterFn realKey) mapA)
        ]
  , env (pure (lenMap dictA)) $ \mapA ->
      bgroup "traversal"
        [ bench "fmap" (nf (fmap (+3)) mapA)
        , bench "foldMap" (nf (foldMap Sum) mapA) ]
  , env (pure $ lenMap dictA) $ \mapA ->
    env (pure $ lenMap dictB) $ \mapB ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry Map.union) (mapA, mapB))
        , bench "intersection" (whnf (uncurry Map.intersection) (mapA, mapB))
        , bench "difference" (whnf (uncurry Map.difference) (mapA, mapB))
        , env (pure $ lenMap gibberish) $ \mapSmall ->
            bench "append" (whnf (uncurry mapProd) (mapSmall, mapSmall)) ]
  ]
  where
    realKey = head dictA
    longKey = concat (replicate 100 realKey)
    shortKey = take 3 realKey

lenMap :: (Ord c) => [[c]] -> Map [c] Int
lenMap dict = Map.fromList [(w, length w) | w <- dict]

mapProd :: (Ord c) => Map [c] Int -> Map [c] Int -> Map [c] Int
mapProd m1 m2 =
  foldl' (Map.unionWith (+)) Map.empty
    [ prod1 s x m2 | (s,x) <- Map.toList m1 ]
  where
    prod1 s x m = Map.mapKeysMonotonic (s++) $ Map.map (x*) m

mapLookupPrefixes :: Ord c => [c] -> Map [c] Int -> [([c], Int)]
mapLookupPrefixes xs m = 
  let m' = Map.takeWhileAntitone (\k -> k <= xs) m
  in mapLookupIncreasingKeys (inits xs) m'

mapLookupIncreasingKeys :: Ord k => [k] -> Map k a -> [(k,a)]
mapLookupIncreasingKeys = go
  where
    go []       _ = []
    go (k:keys) m
      | Map.null m = []
      | otherwise = case Map.splitLookup k m of
          (_, Nothing, m') -> go keys m'
          (_, Just a,  m') -> (k,a) : go keys m'

mapMatch :: (Ord c) => [c] -> Map [c] a -> (Maybe a, Map [c] a)
mapMatch prefix m =
  let n = length prefix
      m' = Map.dropWhileAntitone (< prefix) m
      m'' = Map.takeWhileAntitone (isPrefixOf prefix) m'
  in (Map.lookup prefix m'', Map.mapKeysMonotonic (drop n) m'')
