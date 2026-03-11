module Main where

import qualified Mastermind.Test as Test

import qualified Test.QuickCheck as QC
import System.TimeIt (timeIt)

import Data.Foldable (forM_, )


main :: IO ()
main =
   forM_ Test.tests $ \(name, (_count, prop)) -> do
      putStr $ name ++ ": "
      timeIt $ QC.quickCheckWith (QC.stdArgs {QC.maxSuccess = 1000}) prop

{-
Shorter times without UseSymbol are a bit misleading,
because the ten selected consistentCodes may contain duplicates.

let n = 4
let set = ['a'..'k']
fromList []
+++ OK, passed 1000 tests.
CPU time:   7.36s
+++ OK, passed 1000 tests.
CPU time:  44.06s
fromList [UseSymbolPos]
+++ OK, passed 1000 tests.
CPU time:  21.70s
+++ OK, passed 1000 tests.
CPU time: 134.35s
fromList [UseSymbol]
+++ OK, passed 1000 tests.
CPU time:  12.89s
+++ OK, passed 1000 tests.
CPU time:  38.74s
fromList [UseSymbol,UseSymbolPos]
+++ OK, passed 1000 tests.
CPU time:  33.16s
+++ OK, passed 1000 tests.
CPU time:  81.25s

fromList [UniqueSymbol]
+++ OK, passed 1000 tests.
CPU time:   0.88s
+++ OK, passed 1000 tests.
CPU time:  14.94s
fromList [UniqueSymbol,UseSymbolPos]
+++ OK, passed 1000 tests.
CPU time:  58.38s
+++ OK, passed 1000 tests.
CPU time: 266.46s
fromList [UniqueSymbol,UseSymbol]
+++ OK, passed 1000 tests.
CPU time:   1.34s
+++ OK, passed 1000 tests.
CPU time:   9.20s
fromList [UniqueSymbol,UseSymbol,UseSymbolPos]
+++ OK, passed 1000 tests.
CPU time:  44.60s
+++ OK, passed 1000 tests.
CPU time:  66.19s
-}

{-
With EvalRow (Maybe Eval) we get:
fromList []
+++ OK, passed 1000 tests.
CPU time:   7.89s
+++ OK, passed 1000 tests.
CPU time:  41.88s
fromList [UseSymbol]
+++ OK, passed 1000 tests.
CPU time:  10.57s
+++ OK, passed 1000 tests.
CPU time:  33.61s
fromList [UniqueSymbol]
+++ OK, passed 1000 tests.
CPU time:   0.95s
+++ OK, passed 1000 tests.
CPU time:  16.23s
fromList [UniqueSymbol,UseSymbol]
+++ OK, passed 1000 tests.
CPU time:   1.44s
+++ OK, passed 1000 tests.
CPU time:   9.08s
-}
