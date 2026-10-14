{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_)
import Data.List (foldl', nub)

-- The arguments give the machine two capabilities, and each capability has
-- a worker on its own OS thread. A capability with surplus threads pushes
-- threads to an idle capability. See "Workers" in
-- docs/multi-threaded-rts-design.md.

foreign import ccall unsafe "pthread_self" c_pthread_self :: IO Word

main :: IO ()
main = do
  gate <- newEmptyMVar
  boxes <- forM [1 .. 8 :: Int] $ \factor -> do
    box <- newEmptyMVar
    _ <- forkIO $ do
      -- A thread that waits here can wake on another capability, and it
      -- goes back to its own capability.
      takeMVar gate
      let total = foldl' (+) 0 (map (* factor) [1 .. 20000])
      self <- c_pthread_self
      total `seq` putMVar box (total, self)
    pure box
  forM_ [1 .. 8 :: Int] $ \_ -> putMVar gate ()
  answers <- mapM takeMVar boxes
  mapM_ (print . fst) answers
  putStrLn ("several OS threads: " ++ show (length (nub (map snd answers)) > 1))
