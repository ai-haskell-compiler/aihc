module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Test (isSuccess)

import qualified FreeMonoidal

tests :: Q.Property
tests = FreeMonoidal.tests

main :: IO ()
main = do
  r <- Q.quickCheckWithResult Q.stdArgs{ Q.maxSize = 27, Q.maxSuccess = 1000 } tests
  if isSuccess r
    then exitSuccess
    else exitFailure
