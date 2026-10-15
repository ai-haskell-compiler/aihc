module Main (main) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (foldl')
import System.Mem.StableName (eqStableName, makeStableName)

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
  box <- newEmptyMVar
  spare <- newEmptyMVar
  print (churn 500000)
  -- A young list of thunks goes into an old reference. The promotion
  -- copies it into gen2.
  writeIORef shared (map (+ 1) [1 .. 100 :: Int])
  -- A young list that names a young reference goes into an old reference.
  -- The promotion collects, so every name of the young reference names its
  -- copy, and a later write through it is visible through the old one.
  fresh <- newIORef (5 :: Int)
  writeIORef references [fresh]
  modifyIORef fresh (+ 1)
  print (churn 500000)
  readIORef shared >>= print . sum
  readIORef references >>= mapM readIORef >>= print
  print (sum table)
  print (sum table)
  readIORef fresh >>= print
  -- A put into an old MVar promotes the value in the runtime operation,
  -- and a young reference in it makes the promotion collect.
  counter <- newIORef (40 :: Int)
  putMVar box [counter]
  modifyIORef counter (+ 2)
  takeMVar box >>= mapM readIORef >>= print
  -- The same through the put that never blocks.
  other <- newIORef (1 :: Int)
  stored <- tryPutMVar spare [other]
  modifyIORef other (+ 1)
  takeMVar spare >>= mapM readIORef >>= print . (,) stored
  -- Two stable names of one young object are equal, also when the first
  -- name moves the object into gen2.
  let value = map negate [1 .. 10 :: Int]
  first <- makeStableName value
  second <- makeStableName value
  print (sum value, eqStableName first second)
