{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main where

import Prelude

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Data.Bits
import Data.List (foldl', transpose)
import Data.WideWord (Word128)
import Data.Word (Word16, Word64)
import qualified Gauge as G
import Gauge (Benchmark)
import Type.Reflection

import qualified Data.Enum.Set.Base as E

main :: IO ()
main = G.defaultMain . concat . transpose =<< sequence
    [ benchWord @Word
    , benchWord @Word16
    , benchWord @Word64
    , benchWord @Word128
    ]

benchWord :: ∀ w. (FiniteBits w, NFData w, Num w, Typeable w) => IO [Benchmark]
benchWord = do
    let s      = E.fromFoldable elems      :: E.EnumSet w Int
        s_even = E.fromFoldable elems_even :: E.EnumSet w Int
        s_odd  = E.fromFoldable elems_odd  :: E.EnumSet w Int
    evaluate $ rnf [s, s_even, s_odd]
    return
      [ bench "singleton" (E.singleton :: Int -> E.EnumSet w Int) 2
      , bench "fromFoldable" (E.fromFoldable :: [Int] -> E.EnumSet w Int) elems
      , bench "insert" (ins elems) (E.empty :: E.EnumSet w Int)
      , bench "delete" (del elems) s
      , bench "member" (member elems) s
      , bench "notMember" (notMember elems) s
      , bench "null" E.null s
      , bench "size" E.size s
      , bench "isSubsetOf" (E.isSubsetOf s) s_even
      , bench "union" (E.union s_even) s_odd
      , bench "difference" (E.difference s) s_even
      , bench "symmetricDifference" (E.symmetricDifference s) s_even
      , bench "intersection" (E.intersection s) s_even
      , bench "null.intersection:false" (E.null . E.intersection s) s_even
      , bench "null.intersection:true" (E.null . E.intersection s_odd) s_even
      , bench "filter" (E.filter ((== 0) . (`mod` 2))) s
      , bench "partition" (E.partition ((== 0) . (`mod` 2))) s
      , bench "map" (E.map (+1)) s
      , bench "foldl" (E.foldl (flip (:)) []) s
      , bench "foldl'" (E.foldl' (flip (:)) []) s
      , bench "foldr" (E.foldr (:) []) s
      , bench "foldr'" (E.foldr' (:) []) s
      , bench "foldl1" (E.foldl1 (+)) s
      , bench "foldl1'" (E.foldl1' (+)) s
      , bench "foldr1" (E.foldr1 (+)) s
      , bench "foldr1'" (E.foldr1' (+)) s
      , bench "foldMap" (return :: ∀ a. a -> [a]) s
      , bench "traverse" (return :: ∀ a. a -> [a]) s
      , bench "all" (E.all (/= -1)) s
      , bench "any" (E.any (== -1)) s
      , bench "minimum" E.minimum s
      , bench "maximum" E.maximum s
      , bench "deleteMin" E.deleteMin s
      , bench "deleteMax" E.deleteMax s
      , bench "minView" E.minView s
      , bench "maxView" E.maxView s
      , bench "toList" E.toList s
      ]
  where
    maxVal = 15
    elems = [0..maxVal]
    elems_even = [2,4..maxVal]
    elems_odd = [1,3..maxVal]
    prefix = show (typeRep @w) ++ " "
    bench :: String -> (a -> b) -> a -> G.Benchmark
    bench label f = G.bench (prefix ++ label) . G.whnf f

member :: ∀ w a. (Bits w, Enum a) => [a] -> E.EnumSet w a -> Int
member xs s = foldl' (\n x -> if E.member x s then n + 1 else n) 0 xs

notMember :: ∀ w a. (Bits w, Enum a) => [a] -> E.EnumSet w a -> Int
notMember xs s = foldl' (\n x -> if E.notMember x s then n + 1 else n) 0 xs

ins :: ∀ w a. (Bits w, Enum a) => [a] -> E.EnumSet w a -> E.EnumSet w a
ins xs s = foldl' (flip E.insert) s xs

del :: ∀ w a. (Bits w, Enum a) => [a] -> E.EnumSet w a -> E.EnumSet w a
del xs s = foldl' (flip E.delete) s xs
