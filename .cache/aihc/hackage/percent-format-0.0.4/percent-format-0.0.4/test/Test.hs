-- |
-- Module      : Test
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of PercentFormat.
--
-- Some helper functios to test PercentFormat itself.
module Test where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndices)

reportTests :: [Bool] -> IO ()
reportTests tests =
  case elemIndices False tests of
    [] -> putStrLn "+++ Tests passed!"
    is -> do putStrLn ("*** Failed tests:" ++ show is)
             exitFailure
