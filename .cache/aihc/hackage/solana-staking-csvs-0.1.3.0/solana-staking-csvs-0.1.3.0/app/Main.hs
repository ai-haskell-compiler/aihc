module Main where

import Console.SolanaStaking.Main
    ( getArgs
    , run
    )


main :: IO ()
main = getArgs >>= run
