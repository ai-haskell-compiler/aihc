module Main where

import Multiarg.Examples.Telly

main :: IO ()
main = parse >>= print
