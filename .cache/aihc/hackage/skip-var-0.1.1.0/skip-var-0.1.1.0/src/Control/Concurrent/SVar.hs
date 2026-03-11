-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SVar
-- Copyright   :  (c) Daniel Taskoff, 2018
--                    Georgi Lyubenov, 2018
-- License     :  MIT
--
-- Maintainer  :  daniel.taskoff@gmail.com, godzbanebane@gmail.com
-- Stability   :  experimental
--
-- Implementation of skip variables - a special case of [skip channels]
-- (https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/concurrent-haskell.pdf),
-- the difference being that a value stored in a skip variable can be read at most once, while a skip channel
-- can have multiple readers (i.e. /multiple readers/ can read the same value).
--
-- Writing into a skip variable doesn't block - if there's a value stored, it's overwritten. Reading, on the
-- other hand, blocks, if there isn't a new value since the last read, or if the skip variable is empty.
--
-- === Some examples:
--
-- > s <- newEmptySVar :: IO (SVar Int)
-- > print =<< readSVar s -- blocks
--
-- > s <- newSVar 42 :: IO (SVar Int)
-- > print =<< readSVar s -- prints 42
-- > print =<< readSVar s -- blocks
--
-- The next few lines will print some of the numbers between 0 and 10000. Note that at least one number will be
-- printed, but many will be skipped, because 'print'ing is slow.
--
-- > import Control.Concurrent (forkIO, killThread)
-- > import Control.Monad (forever)
-- >
-- > s <- newEmptySVar :: IO (SVar Int)
-- > tid <- forkIO $ forever $ print =<< readSVar s
-- > mapM_ (putSVar s) [0..10000]
-- > killThread tid
-----------------------------------------------------------------------------

module Control.Concurrent.SVar
  (
  -- * 'SVar'
    SVar
  -- ** Creation of 'SVar's
  , newEmptySVar, newSVar
  -- ** Modification of 'SVar's
  , putSVar, readSVar
  ) where

import Control.Concurrent.MVar
  ( MVar, newEmptyMVar, newMVar
  , putMVar, takeMVar
  )


-- | An 'SVar' (skip variable) is a variable which allows for non-blocking updates,
-- and blocking reads if the stored data has been read already, or if there is no data.
data SVar a = SVar (MVar (a, Maybe (MVar ()))) (MVar ())

-- | Create an empty 'SVar'.
newEmptySVar :: IO (SVar a)
newEmptySVar = do
  lock <- newEmptyMVar
  var <- newMVar (error "attempt to read from an empty SVar", Just lock)
  -- lock the 'error' value,
  -- and notify the writer to unlock the next value written
  -- Note: the error will never be evaluated, because 'putSVar' will discard it,
  -- and a 'readSVar' will block until a new value has been written
  pure (SVar var lock)

-- | Create an 'SVar' which contains the supplied value.
newSVar :: a -> IO (SVar a)
newSVar value = do
  lock <- newMVar ()
  -- keep the value unlocked for reading
  var <- newMVar (value, Nothing)
  pure (SVar var lock)

-- | Put a value into an 'SVar'.
-- Never blocks, always overwrites the current value, if there is one.
putSVar :: SVar a -> a -> IO ()
putSVar (SVar var _) value = do
  (_, mlock) <- takeMVar var
  putMVar var (value, Nothing)

  mapM_ (\lock -> putMVar lock ()) mlock

-- | Read a value from an 'SVar'.
-- Blocks if there isn't a new value since the last read, or if the 'SVar' is empty.
readSVar :: SVar a -> IO a
readSVar (SVar var lock) = do
  takeMVar lock

  (value, _) <- takeMVar var
  putMVar var (error "attempt to consume a value more than once", Just lock)
  -- drop the reference to the value,
  -- and notify the writer to unlock the next value written
  -- Note: the error will never be evaluated, because 'putSVar' will discard it,
  -- and a second 'readSVar' will block until a new value has been written

  pure value
