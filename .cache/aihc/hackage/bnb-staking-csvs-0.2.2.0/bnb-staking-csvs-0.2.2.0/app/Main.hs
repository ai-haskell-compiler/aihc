module Main where

import Console.BnbStaking.Main
    ( getArgs
    , run
    )


main :: IO ()
main = getArgs >>= run
