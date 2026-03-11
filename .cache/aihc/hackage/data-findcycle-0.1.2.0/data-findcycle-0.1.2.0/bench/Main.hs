{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

import Control.DeepSeq
import Data.Bits ((.&.))
import Data.FindCycle
import Data.Foldable (find)
import Data.List (intercalate)
import Data.Maybe
import Data.Numbers.Primes
import Data.Word
import GHC.Generics
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer

data CycleSpec = CycleSpec {cycMu, cycLambda, cycDelay :: Int}

instance Show CycleSpec where
    show CycleSpec{..} =
        intercalate
            ","
            [ showParam "mu" cycMu
            , showParam "lambda" cycLambda
            , showParam "delay" cycDelay
            ]
      where
        showParam s v = concat [s, "=", show v]

data Cycle = Cycle {cycF :: Int -> Int, cycX0 :: Int}
    deriving (Generic, NFData)

mkCycle :: CycleSpec -> Cycle
mkCycle CycleSpec{..} = Cycle (delayed cycDelay f) (g 0)
  where
    n = cycMu + cycLambda
    m = fromJust $ find (> n) primes
    a = 123457
    b = 98765
    g i = (a * i + b) `mod` m
    f x
        | i < n - 1 = g (i + 1)
        | otherwise = g cycMu
      where
        i = (modInv a m * ((x - b) `mod` m)) `mod` m
    modInv 1 _ = 1
    modInv x y = (i * y + 1) `div` x
      where
        i = x - modInv (y `mod` x) x

{-# NOINLINE delayed #-}
delayed :: Int -> (a -> b) -> (a -> b)
delayed n f x = countTo n `seq` f x
  where
    countTo 0 = ()
    countTo i = countTo (i - 1)

main :: IO ()
main = defaultMain [mapLeafBenchmarks compareBrent benchmark]
  where
    compareBrent (name : xs)
        | name /= "brent" = bcompare (printAwkExpr (locateBenchmark ("brent" : xs)))
    compareBrent _ = id

cycles :: [CycleSpec]
cycles =
    [ CycleSpec mu lambda delay
    | mu <- [0, 10000, 1000000]
    , lambda <- [10000, 1000000]
    , delay <- [0, 10, 100, 1000]
    ]

runners :: [(String, CycleFinder Int -> Cycle -> Benchmarkable)]
runners =
    [ ("findCycle", nf . runAlg findCycle)
    , ("findExtractCycle", nf . runAlg findCycleExtract)
    , ("findExtractCycle+drop", nf . (dropLists .) . runAlg findCycleExtract)
    ,
        ( "unsafeFindCycleFromList"
        , nf . runAlg (\alg -> (unsafeFindCycleFromList alg .) . iterate)
        )
    ]
  where
    runAlg f alg Cycle{..} = f alg cycF cycX0
    dropLists (a, b, _) = (a, b)

algs :: [(String, CycleFinder Int)]
algs =
    [ ("brent", brent)
    , ("floyd", floyd)
    , ("nivasch", nivasch)
    , ("nivaschPart", nivaschPart (fromIntegral :: Int -> Word8))
    , ("nivaschPartWithinBounds", nivaschPartWithinBounds (0, 0xff) (.&. 0xff))
    , ("naiveHashable", naiveHashable)
    , ("naiveOrd", naiveOrd)
    ]

benchmark :: Benchmark
benchmark =
    bgroup
        "Data.FindCycle"
        [ bgroup
            rName
            [ env (pure (mkCycle spec)) $ \cyc ->
                bgroup
                    (show spec)
                    [ bench name (rf alg cyc)
                    | (name, alg) <- algs
                    ]
            | spec <- cycles
            ]
        | (rName, rf) <- runners
        ]
