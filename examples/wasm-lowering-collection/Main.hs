-- On wasm32-wasip3, each open of a file lowers the list of preopened
-- directories into the program. The harness gives a directory with a long
-- name, so each lowering allocates much memory, and some allocation starts
-- a collection. The collection must not call an import, because the
-- canonical ABI forbids that during a lowering.
module Main where

import System.Environment (getArgs)
import System.IO (readFile')

main :: IO ()
main = do
  arguments <- getArgs
  path <- case arguments of
    ["--read", directory] -> pure (directory ++ "/corpus.txt")
    _ -> do
      writeFile "corpus.txt" "hello corpus\n"
      pure "corpus.txt"
  lengths <- mapM (\_ -> length <$> readFile' path) [1 .. 5000 :: Int]
  print (sum lengths)
