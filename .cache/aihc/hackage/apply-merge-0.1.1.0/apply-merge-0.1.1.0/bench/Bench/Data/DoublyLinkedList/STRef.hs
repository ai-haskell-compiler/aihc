-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Data.DoublyLinkedList.STRef (benchmarks) where

import Control.Monad (forM_, void)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.DoublyLinkedList.STRef
  ( cons,
    delete,
    empty,
    head,
    insertAfter,
    prev,
    snoc,
    value,
  )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)
import Prelude hiding (head, last, null)

superscript :: Char -> Char
superscript c = fromMaybe c (Map.lookup c superscriptMap)

superscriptMap :: Map Char Char
superscriptMap =
  Map.fromList
    [ ('0', '⁰'),
      ('1', '¹'),
      ('2', '²'),
      ('3', '³'),
      ('4', '⁴'),
      ('5', '⁵'),
      ('6', '⁶'),
      ('7', '⁷'),
      ('8', '⁸'),
      ('9', '⁹')
    ]

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Data.DoublyLinkedList.STRef"
    [ constructionBenchmarks,
      traversalBenchmarks,
      queryBenchmarks,
      insertionBenchmarks,
      deletionBenchmarks,
      listConversionBenchmarks
    ]

-- * Construction

constructionBenchmarks :: Benchmark
constructionBenchmarks =
  bgroup
    "Construction"
    [ emptyBenchmarks,
      fromListBenchmarks
    ]

emptyBenchmarks :: Benchmark
emptyBenchmarks = bgroup "empty" []

fromListBenchmarks :: Benchmark
fromListBenchmarks = bgroup "fromList" []

-- * Traversal

traversalBenchmarks :: Benchmark
traversalBenchmarks =
  bgroup
    "Traversal"
    [ headBenchmarks,
      lastBenchmarks,
      nextBenchmarks,
      prevBenchmarks
    ]

headBenchmarks :: Benchmark
headBenchmarks = bgroup "head" []

lastBenchmarks :: Benchmark
lastBenchmarks = bgroup "last" []

nextBenchmarks :: Benchmark
nextBenchmarks = bgroup "next" []

prevBenchmarks :: Benchmark
prevBenchmarks = bgroup "prev" []

-- * Query

queryBenchmarks :: Benchmark
queryBenchmarks = bgroup "Query" [nullBenchmarks, valueBenchmarks]

nullBenchmarks :: Benchmark
nullBenchmarks = bgroup "null" []

valueBenchmarks :: Benchmark
valueBenchmarks = bgroup "value" []

-- * Insertion

insertionBenchmarks :: Benchmark
insertionBenchmarks =
  bgroup
    "Insertion"
    [ consBenchmarks,
      snocBenchmarks,
      insertAfterBenchmarks,
      insertBeforeBenchmarks
    ]

consBenchmarks :: Benchmark
consBenchmarks = bgroup "cons" $
  flip map [(1 :: Int) .. 6] $ \i ->
    bench ("cons 10" ++ map superscript (show i) ++ " times") $
      flip nf ((10 :: Int) ^ i) $ \n -> runST $ do
        list <- empty
        forM_ [1 .. n] (cons list)
        firstNode <- head list
        let firstValue = value <$> firstNode
        pure firstValue

snocBenchmarks :: Benchmark
snocBenchmarks = bgroup "snoc" $
  flip map [(1 :: Int) .. 6] $ \i ->
    bench ("snoc 10" ++ map superscript (show i) ++ " times") $
      flip nf ((10 :: Int) ^ i) $ \n -> runST $ do
        list <- empty
        forM_ [1 .. n] (snoc list)
        firstNode <- head list
        let firstValue = value <$> firstNode
        pure firstValue

insertBeforeBenchmarks :: Benchmark
insertBeforeBenchmarks = bgroup "insertBefore" []

insertAfterBenchmarks :: Benchmark
insertAfterBenchmarks = bgroup "insertAfter" []

-- * Deletion

deletionBenchmarks :: Benchmark
deletionBenchmarks = bgroup "Deletion" [deleteBenchmarks]

deleteBenchmarks :: Benchmark
deleteBenchmarks = bgroup "delete (uses insertAfter and prev)" $
  flip map [(1 :: Int) .. 6] $ \i ->
    bench ("delete 10" ++ map superscript (show i) ++ " times") $
      flip nf ((10 :: Int) ^ i) $ \n -> runST $ runMaybeT $ do
        list <- lift empty
        node1 <- lift (cons list (1 :: Int))
        node2 <- lift (insertAfter node1 2)
        node3 <- lift (insertAfter node2 3)
        forM_ [1 .. n] $ \j -> do
          lift (void (insertAfter node1 j))
          prevNode <- MaybeT (prev node3)
          lift (delete prevNode)

-- * List conversion

listConversionBenchmarks :: Benchmark
listConversionBenchmarks = bgroup "List conversion" [toListBenchmarks]

toListBenchmarks :: Benchmark
toListBenchmarks = bgroup "toList" []
