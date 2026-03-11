-- Copyright (c) 2015-2022 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import System.Exit (exitFailure)
import Data.List (elemIndices)
import Data.Text

import Test.LeanCheck
import Test.LeanCheck.Instances
import Prelude hiding (lines, unlines, words, unwords)

main :: IO ()
main =
  case elemIndices False (tests 1000) of
  [] -> putStrLn "Tests passed!"
  is -> putStrLn ("Failed tests:" ++ Prelude.show is) >> exitFailure

tests :: Int -> [Bool]
tests n =
  [ True
  , fails n $ \ts -> lines (unlines ts) == ts
  , fails n $ \t ->   unlines (lines t) == t
  , holds n $ \ts -> unlines (lines (unlines ts)) == unlines ts
  , holds n $ \ts ->   lines (unlines (lines ts)) == lines ts

  , fails n $ \ts -> words (unwords ts) == ts
  , fails n $ \t ->   unwords (words t) == t
  , holds n $ \ts ->   words (unwords (words ts)) == words ts

  , counterExample n (\ts -> words (unwords ts) == ts)
    == Just ["[\"\"]"]
  ]
