module Main where

import Specs (tests)
import qualified System.Exit as Exit

main :: IO ()
main = do
  b <- tests
  if b then Exit.exitSuccess else Exit.exitFailure
