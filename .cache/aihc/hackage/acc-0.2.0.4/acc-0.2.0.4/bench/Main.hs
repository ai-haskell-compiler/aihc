module Main where

import qualified Acc
import Criterion
import Criterion.Main
import qualified Data.DList as DList
import qualified Data.Sequence as Sequence
import Prelude

main :: IO ()
main =
  defaultMain
    [ bgroup "sum"
        $ [ onIntListByMagBench "cons" 4 $ \input ->
              [ reduceConstructBench "acc" input sum
                  $ foldl' (flip Acc.cons) mempty,
                reduceConstructBench "list" input sum
                  $ foldl' (flip (:)) [],
                reduceConstructBench "dlist" input sum
                  $ foldl' (flip DList.cons) mempty,
                reduceConstructBench "sequence" input sum
                  $ foldl' (flip (Sequence.<|)) mempty
              ],
            onIntListByMagBench "snoc" 4 $ \input ->
              [ reduceConstructBench "acc" input sum
                  $ foldl' (flip Acc.snoc) mempty,
                reduceConstructBench "dlist" input sum
                  $ foldl' DList.snoc mempty,
                reduceConstructBench "sequence" input sum
                  $ foldl' (Sequence.|>) mempty
              ],
            onIntListByMagBench "fromList" 4 $ \input ->
              [ reduceConstructBench "acc" input sum $ fromList @(Acc.Acc Int),
                reduceConstructBench "list" input sum $ id,
                reduceConstructBench "dlist" input sum $ DList.fromList,
                reduceConstructBench "sequence" input sum $ Sequence.fromList
              ],
            bgroup "append"
              $ [ bgroup "left"
                    $ onIntListByMagBenchList 4
                    $ \input ->
                      onSizeByMagBenchList 4 $ \appendAmount ->
                        [ appendLeftBench "acc" appendAmount (fromList @(Acc.Acc Int) input) sum,
                          appendLeftBench "list" appendAmount input sum,
                          appendLeftBench "dlist" appendAmount (DList.fromList input) sum,
                          appendLeftBench "sequence" appendAmount (Sequence.fromList input) sum
                        ],
                  bgroup "right"
                    $ onIntListByMagBenchList 4
                    $ \input ->
                      onSizeByMagBenchList 4 $ \appendAmount ->
                        [ appendRightBench "acc" appendAmount (fromList @(Acc.Acc Int) input) sum,
                          appendRightBench "list" appendAmount input sum,
                          appendRightBench "dlist" appendAmount (DList.fromList input) sum,
                          appendRightBench "sequence" appendAmount (Sequence.fromList input) sum
                        ]
                ]
          ],
      bgroup "length"
        $ [ onIntListByMagBench "cons" 4 $ \input ->
              [ reduceConstructBench "acc" input length
                  $ foldl' (flip Acc.cons) mempty,
                reduceConstructBench "list" input length
                  $ foldl' (flip (:)) [],
                reduceConstructBench "dlist" input length
                  $ foldl' (flip DList.cons) mempty,
                reduceConstructBench "sequence" input length
                  $ foldl' (flip (Sequence.<|)) mempty
              ]
          ]
    ]

-- |
-- Construct a benchmark that measures construction of the intermediate representation
-- and reduction, ensuring that they don't get fused.
{-# NOINLINE reduceConstructBench #-}
reduceConstructBench ::
  (NFData reduction, NFData a) =>
  -- | Benchmark name.
  String ->
  -- | Input sample.
  [a] ->
  -- | Reducer of the intermediate representation.
  (intermediate -> reduction) ->
  -- | Constructor of the intermediate representation.
  ([a] -> intermediate) ->
  Benchmark
reduceConstructBench name list reducer constructor =
  bench name $ nf (reducer . constructor) $!! list

-- |
-- Construct a benchmark that measures appending from the left side of a
-- preconstructed chunk of an intermediate representation.
{-# NOINLINE appendLeftBench #-}
appendLeftBench ::
  (NFData reduction, NFData intermediate, Monoid intermediate) =>
  -- | Benchmark name.
  String ->
  -- | How many appends.
  Int ->
  -- | Sample intermediate representation.
  intermediate ->
  -- | Reducer of the intermediate representation.
  (intermediate -> reduction) ->
  Benchmark
appendLeftBench name appendAmount chunk reducer =
  let input =
        replicate appendAmount chunk
   in reduceConstructBench name input reducer
        $ foldl' (flip (<>)) mempty

-- |
-- Construct a benchmark that measures appending from the right side of a
-- preconstructed chunk of an intermediate representation.
{-# NOINLINE appendRightBench #-}
appendRightBench ::
  (NFData reduction, NFData intermediate, Monoid intermediate) =>
  -- | Benchmark name.
  String ->
  -- | How many appends.
  Int ->
  -- | Sample intermediate representation.
  intermediate ->
  -- | Reducer of the intermediate representation.
  (intermediate -> reduction) ->
  Benchmark
appendRightBench name appendAmount chunk reducer =
  let input =
        replicate appendAmount chunk
   in reduceConstructBench name input reducer
        $ foldl' (<>) mempty

onIntListByMagBench :: String -> Int -> ([Int] -> [Benchmark]) -> Benchmark
onIntListByMagBench groupName amount benchmarks =
  onSizeByMagBench groupName amount $ \size ->
    benchmarks $!! enumFromTo 0 size

onSizeByMagBench :: String -> Int -> (Int -> [Benchmark]) -> Benchmark
onSizeByMagBench groupName amount benchmarks =
  bgroup groupName $ onSizeByMagBenchList amount benchmarks

onIntListByMagBenchList :: Int -> ([Int] -> [Benchmark]) -> [Benchmark]
onIntListByMagBenchList amount benchmarks =
  onSizeByMagBenchList amount $ \size ->
    benchmarks $!! enumFromTo 0 size

onSizeByMagBenchList :: Int -> (Int -> [Benchmark]) -> [Benchmark]
onSizeByMagBenchList amount benchmarks =
  take amount sizesByMagnitude <&> \size -> bgroup (show size) (benchmarks size)

sizesByMagnitude :: [Int]
sizesByMagnitude = [0 :: Int ..] <&> \magnitude -> 10 ^ magnitude
