module Main (main) where

import Callback (callExports)

main :: IO ()
main = callExports 21 >>= print
