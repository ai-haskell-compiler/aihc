module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified ParserBench
import qualified WriterBench

main :: IO ()
main = defaultMain
    [ bgroup "Parser" ParserBench.benchmarks
    , bgroup "Writer" WriterBench.benchmarks
    ]
