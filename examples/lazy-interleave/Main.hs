module Main where

import Control.Monad.ST (runST)
import GHC.ST (unsafeInterleaveST)
import System.IO.Unsafe (unsafeInterleaveIO)

-- unsafeInterleaveST goes through noDuplicate#, which the native backends
-- lower to nothing because a computation is never duplicated here.
main :: IO ()
main = do
  print (runST (unsafeInterleaveST (return (1 :: Int))))
  deferred <- unsafeInterleaveIO (return (2 :: Int))
  print deferred
