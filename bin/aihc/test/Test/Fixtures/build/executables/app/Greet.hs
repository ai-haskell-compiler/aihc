module Main (main) where

import Words (greeting)

main :: IO ()
main = putStrLn (greeting "build")
