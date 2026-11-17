module Main (main) where

import Control.Concurrent (getNumCapabilities)

main :: IO ()
main = getNumCapabilities >>= print
