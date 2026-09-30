module Main (main) where

import Loud (shout)

main :: IO ()
main = putStrLn (shout "build")
