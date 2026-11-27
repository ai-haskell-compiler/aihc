module Main (main) where

import Clock (afterEpoch)

main :: IO ()
main = afterEpoch >>= print
