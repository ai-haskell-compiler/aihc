module Main (main) where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (foldl')

-- The arguments give the machine two capabilities. A store of a young value
-- into a global object is then an escape, and the runtime promotes the
-- value into gen2. See "Local heaps" in docs/multi-threaded-rts-design.md.

-- Allocate enough to run several collections, so the older references
-- reach gen2.
churn :: Int -> Int
churn count = foldl' (+) 0 (map (`mod` 7) [1 .. count])

-- A top-level value: its update stores a young list into a static object.
table :: [Int]
table = map (* 2) [1 .. 1000]

main :: IO ()
main = do
  shared <- newIORef []
  references <- newIORef []
  print (churn 2000000)
  -- A young list of thunks goes into an old reference. The promotion
  -- copies it into gen2.
  writeIORef shared (map (+ 1) [1 .. 100 :: Int])
  -- A young list that names a young reference goes into an old reference.
  -- The promotion collects, so every name of the young reference names its
  -- copy, and a later write through it is visible through the old one.
  fresh <- newIORef (5 :: Int)
  writeIORef references [fresh]
  modifyIORef fresh (+ 1)
  print (churn 2000000)
  readIORef shared >>= print . sum
  readIORef references >>= mapM readIORef >>= print
  print (sum table)
  print (sum table)
  readIORef fresh >>= print
