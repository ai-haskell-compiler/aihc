module Main where

import Control.Monad (unless)
import Hedgehog (checkParallel)
-- import Hedgehog (checkSequential)
import System.Exit (exitFailure)

import qualified Data.EDN.AST.Test as AST
import qualified Data.EDN.Class.Test as Class
import qualified Data.EDN.QQ.Test as QQ

main :: IO ()
main = do
  putStrLn ""
  ok <- tests
  unless ok exitFailure

tests :: IO Bool
tests = fmap and . sequence $ map checkParallel
  [ AST.tests
  , Class.tests
  , QQ.tests
  ]
