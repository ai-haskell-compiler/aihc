-- |Implements bounded channels. These channels differ from normal 'Chan's in
-- that they are guaranteed to contain no more than a certain number of
-- elements. This is ideal when you may be writing to a channel faster than you
-- are able to read from it.
--
-- This module supports all the functions of "Control.Concurrent.Chan" except
-- 'unGetChan' and 'dupChan', which are not supported for bounded channels.
--
-- Extra consitency: This version enforces that if thread Alice writes
-- e1 followed by e2 then e1 will be returned by readChan before e2.
-- Conversely, if thead Bob reads e1 followed by e2 then it was true that
-- writeChan e1 preceded writeChan e2.
--
-- Previous versions did not enforce this consistency: if writeChan were
-- preempted between putMVars or killThread arrived between putMVars then it
-- can fail.  Similarly it might fail if readChan were stopped after putMVar
-- and before the second takeMVar.  An unlucky pattern of several such deaths
-- might actually break the invariants of the array in an unrecoverable way
-- causing all future reads and writes to block.
module Control.Concurrent.BoundedChan(
         BoundedChan
       , newBoundedChan
       , writeChan
       , tryWriteChan
       , readChan
       , tryReadChan
       , isEmptyChan
       , getChanContents
       , writeList2Chan
       )
  where

import Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, newMVar,
                                putMVar, tryPutMVar, takeMVar, tryTakeMVar)
import Control.Exception       (mask_, onException)
import Control.Monad           (replicateM)
import Data.Array              (Array, (!), listArray)
import System.IO.Unsafe        (unsafeInterleaveIO)

-- |'BoundedChan' is an abstract data type representing a bounded channel.
data BoundedChan a = BC {
       _size     :: Int
     , _contents :: Array Int (MVar a)
     , _writePos :: MVar Int
     , _readPos  :: MVar Int
     }

-- Versions of modifyMVar and withMVar that do not 'restore' the previous mask state when running
-- 'io', with added modification strictness.  The lack of 'restore' may make these perform better
-- than the normal version.  Moving strictness here makes using them more pleasant.
{-# INLINE modifyMVar_mask #-}
modifyMVar_mask :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar_mask m io =
  mask_ $ do
    a <- takeMVar m
    (a',b) <- io a `onException` putMVar m a
    putMVar m $! a'
    return b

{-# INLINE modifyMVar_mask_ #-}
modifyMVar_mask_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_mask_ m io =
  mask_ $ do
    a <- takeMVar m
    a' <- io a `onException` putMVar m a
    putMVar m $! a'

{-# INLINE withMVar_mask #-}
withMVar_mask :: MVar a -> (a -> IO b) -> IO b
withMVar_mask m io =
  mask_ $ do
    a <- takeMVar m
    b <- io a `onException` putMVar m a
    putMVar m a
    return b

-- |@newBoundedChan n@ returns a channel than can contain no more than @n@
-- elements.
newBoundedChan :: Int -> IO (BoundedChan a)
newBoundedChan x = do
  entls <- replicateM x newEmptyMVar
  wpos  <- newMVar 0
  rpos  <- newMVar 0
  let entries = listArray (0, x - 1) entls
  return (BC x entries wpos rpos)

-- |Write an element to the channel. If the channel is full, this routine will
-- block until it is able to write.  Blockers wait in a fair FIFO queue.
writeChan :: BoundedChan a -> a -> IO ()
writeChan (BC size contents wposMV _) x = modifyMVar_mask_ wposMV $
  \wpos -> do
    putMVar (contents ! wpos) x
    return ((succ wpos) `mod` size) -- only advance when putMVar succeeds

-- |A variant of 'writeChan' which, instead of blocking when the channel is
-- full, simply aborts and does not write the element. Note that this routine
-- can still block while waiting for write access to the channel.
tryWriteChan :: BoundedChan a -> a -> IO Bool
tryWriteChan (BC size contents wposMV _) x = modifyMVar_mask wposMV $
  \wpos -> do
    success <- tryPutMVar (contents ! wpos) x
    return $ if success
      then ((succ wpos) `mod` size, True) -- only advance when putMVar succeeds
      else (wpos, False)

-- |Read an element from the channel. If the channel is empty, this routine
-- will block until it is able to read.  Blockers wait in a fair FIFO queue.
readChan :: BoundedChan a -> IO a
readChan (BC size contents _ rposMV) = modifyMVar_mask rposMV $
  \rpos -> do
    a <- takeMVar (contents ! rpos)
    return ((succ rpos) `mod` size, a) -- only advance when takeMVar succeeds

-- |A variant of 'readChan' which, instead of blocking when the channel is
-- empty, immediately returns 'Nothing'. Otherwise, 'tryReadChan' returns
-- @'Just' a@ where @a@ is the element read from the channel. Note that this
-- routine can still block while waiting for read access to the channel.
tryReadChan :: BoundedChan a -> IO (Maybe a)
tryReadChan (BC size contents _ rposMV) = modifyMVar_mask rposMV $
  \rpos -> do
    ma <- tryTakeMVar (contents ! rpos)
    return $ case ma of
      Just a -> ((succ rpos) `mod` size, Just a) -- only advance when takeMVar succeeds
      Nothing -> (rpos, Nothing)

-- |DANGER: This may block on an empty channel if there is already a blocked reader.
-- Returns 'True' if the supplied channel is empty.
--
-- DEPRECATED
{-# DEPRECATED isEmptyChan "This isEmptyChan can block, no non-blocking substitute yet" #-}
isEmptyChan :: BoundedChan a -> IO Bool
isEmptyChan (BC _ contents _ rposMV) = withMVar_mask rposMV $
  \rpos -> isEmptyMVar (contents ! rpos)

-- |Return a lazy list representing the contents of the supplied channel.  Competing
-- readers might steal from this list.
getChanContents :: BoundedChan a -> IO [a]
getChanContents ch = unsafeInterleaveIO $ do
  x  <- readChan ch
  xs <- getChanContents ch
  return (x:xs)

-- |Write a list of elements to the channel. If the channel becomes full, this
-- routine will block until it is able to write.  Competing writers may interleave with
-- this one.
writeList2Chan :: BoundedChan a -> [a] -> IO ()
writeList2Chan = mapM_ . writeChan
