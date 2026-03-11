-- test-list.hs -- example program, testing Data.List using LeanCheck
--
-- Copyright (c) 2017-2022 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Instances
import Data.Char
import Data.Text as T
import Prelude as P hiding (lines, unlines, words, unwords)

main :: IO ()
main = do
  -- wrong: append is clearly not commutative
  check $ \t1 t2 -> t1 `append` t2 == t2 `append` t1

  -- wrong: lines and unlines do not form an identity
  check $ \ts -> lines (unlines ts) == ts
  check $ \t ->   unlines (lines t) == t

  -- correct: lines and unlines form an identity under certain conditions
  check $ \ts -> P.all (T.all isAlphaNum) ts
             ==> lines (unlines ts) == ts
  check $ \ts -> unlines (lines (unlines ts)) == unlines ts
  check $ \ts ->   lines (unlines (lines ts)) == lines ts

  -- wrong: words and unwords do not form an identity
  check $ \ts -> words (unwords ts) == ts
  check $ \t ->   unwords (words t) == t

  -- correct: words and unwords form an identity under certain conditions
  check $ \ts ->   words (unwords (words ts)) == words ts
