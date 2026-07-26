module Main where

import System.IO (IOMode (ReadMode), openBinaryFile)

main :: IO ()
main = do
  openBinaryFile "missing-system-io-file" ReadMode
  return ()
