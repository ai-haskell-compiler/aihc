module Main (main) where

import Criterion.Main

import Control.Monad.Writer
import Data.DoList
import Data.DoMonoid

import Data.Text as T (Text, unlines)

main :: IO ()
main = defaultMain $ toList $ do
  benchGroup "Sum" $ do
    whnfBench "Writer"   (sumWriter 1 2 3 4) 5
    whnfBench "DoList"   (sumList   1 2 3 4) 5
    whnfBench "DoMonoid" (sumMonoid 1 2 3 4) 5
  benchGroup "List" $ do
    whnfBench "Writer" (last . listWriter 1 2 3 4) 5
    whnfBench "DoList" (last . listList   1 2 3 4) 5
  benchGroup "Lines" $ do
    whnfBench "Writer" (linesWriter "Line 1\n" "Line 2\n") "Line 3\n"
    whnfBench "List"   (linesList   "Line 1"   "Line 2"  ) "Line 3"
    whnfBench "Monoid" (linesMonoid "Line 1\n" "Line 2\n") "Line 3\n"


sumWriter, sumList, sumMonoid :: Int -> Int -> Int -> Int -> Int -> Sum Int

{-# NOINLINE sumWriter #-}
sumWriter x1 x2 x3 x4 x5 = execWriter $ do
  tell $ Sum x1
  tell $ Sum x2
  tell $ Sum x3
  tell $ Sum x4
  tell $ Sum x5

{-# NOINLINE sumList #-}
sumList x1 x2 x3 x4 x5 = mconcat $ toList $ do
  item $ Sum x1
  item $ Sum x2
  item $ Sum x3
  item $ Sum x4
  item $ Sum x5

{-# NOINLINE sumMonoid #-}
sumMonoid x1 x2 x3 x4 x5 = runDoM $ do
  DoM $ Sum x1
  DoM $ Sum x2
  DoM $ Sum x3
  DoM $ Sum x4
  DoM $ Sum x5


listWriter, listList :: Int -> Int -> Int -> Int -> Int -> [Int]

{-# NOINLINE listWriter #-}
listWriter x1 x2 x3 x4 x5 = execWriter $ do
  tell [x1]
  tell [x2]
  tell [x3]
  tell [x4]
  tell [x5]

{-# NOINLINE listList #-}
listList x1 x2 x3 x4 x5 = toList $ do
  item x1
  item x2
  item x3
  item x4
  item x5


linesWriter, linesList, linesMonoid :: Text -> Text -> Text -> Text

{-# NOINLINE linesWriter #-}
linesWriter x1 x2 x3 = execWriter $ do
  tell x1
  tell x2
  tell x3

{-# NOINLINE linesList #-}
linesList x' x2 x3 = T.unlines $ toList $ do
  item x'
  item x2
  item x3

{-# NOINLINE linesMonoid #-}
linesMonoid x1 x2 x3 = runDoM $ do
  DoM x1
  DoM x2
  DoM x3


benchGroup :: String -> DoList Benchmark -> DoList Benchmark
benchGroup name = item . bgroup name . toList

whnfBench :: String -> (a -> b) -> a -> DoList Benchmark
whnfBench name func = item . bench name . whnf func
