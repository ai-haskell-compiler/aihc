{-| This module wraps Control.Concurrent.Thread.Group and provides a bounded 
    version of ThreadGroup, mainly 'BoundedThreadGroup'.
    
    In addition to the functionality of 'ThreadGroup', 'BoundedThreadGroup'
    will block creation of threads until there fewer than the given 
    max size.
-}
{-# LANGUAGE RecordWildCards, RankNTypes #-}
module Control.Concurrent.Thread.BoundedThreadGroup where
import Control.Concurrent.Thread.Group (ThreadGroup)
import qualified Control.Concurrent.Thread as Thread
import qualified Control.Concurrent.Thread.Group as ThreadGroup
import Control.Concurrent
import Control.Concurrent.STM

-- | A 'BoundedThreadGroup' extends the concept of a 'ThreadGroup' by
--   restricting the number of active threads to a given max size.
--   If one attempts to create more than the max size number of threads
--   the fork function will block until threads finish.
data BoundedThreadGroup = BoundedThreadGroup 
    { maxSize     :: Int
    , threadGroup :: ThreadGroup
    }

-- | Create a new 'BoundedThreadGroup' with the passed in max size
new :: Int -> IO BoundedThreadGroup
new maxSize = do 
  threadGroup <- ThreadGroup.new 
  return BoundedThreadGroup {..}

-- | Same as Control.Concurrent.Thread.Group.nrOfRunning
nrOfRunning :: BoundedThreadGroup -> STM Int
nrOfRunning BoundedThreadGroup {..} = ThreadGroup.nrOfRunning threadGroup

-- | Same as Control.Concurrent.Thread.Group.wait
wait :: BoundedThreadGroup -> IO ()
wait BoundedThreadGroup {..} = ThreadGroup.wait threadGroup

-- | Same as Control.Concurrent.Thread.Group.waitN
waitN :: Int -> BoundedThreadGroup -> IO ()
waitN i BoundedThreadGroup {..} = ThreadGroup.waitN i threadGroup

-- | Same as Control.Concurrent.Thread.Group.forkIO but waits there are less
--   then the max size number of threads.
forkIO :: BoundedThreadGroup -> IO a -> IO (ThreadId, IO (Thread.Result a))
forkIO BoundedThreadGroup {..} action = do
  ThreadGroup.waitN maxSize threadGroup
  ThreadGroup.forkIO threadGroup action 

-- | Same as Control.Concurrent.Thread.Group.forkOS but waits there are less
--   then the max size number of threads.
forkOS :: BoundedThreadGroup -> IO a -> IO (ThreadId, IO (Thread.Result a))
forkOS BoundedThreadGroup {..} action = do
  ThreadGroup.waitN maxSize threadGroup
  ThreadGroup.forkOS threadGroup action 

-- | Same as Control.Concurrent.Thread.Group.forkOn but waits there are less
--   then the max size number of threads.
forkOn :: Int 
       -> BoundedThreadGroup 
       -> IO a 
       -> IO (ThreadId, IO (Thread.Result a))
forkOn i BoundedThreadGroup {..} action = do
  ThreadGroup.waitN maxSize threadGroup
  ThreadGroup.forkOn i threadGroup action

-- | Same as Control.Concurrent.Thread.Group.forkIOWithUnmask 
--   but waits there are less then the max size number of threads.
forkIOWithUnmask :: BoundedThreadGroup
                 -> ((forall b. IO b -> IO b) -> IO a)
                 -> IO (ThreadId, IO (Thread.Result a))
forkIOWithUnmask BoundedThreadGroup {..} f = do 
  ThreadGroup.waitN maxSize threadGroup
  ThreadGroup.forkIOWithUnmask threadGroup f

-- | Same as Control.Concurrent.Thread.Group.forkOnWithUnmask 
--   but waits there are less then the max size number of threads.
forkOnWithUnmask :: Int 
                 -> BoundedThreadGroup 
                 -> ((forall b. IO b -> IO b) -> IO a) 
                 -> IO (ThreadId, IO (Thread.Result a))
forkOnWithUnmask i BoundedThreadGroup {..} f = do 
  ThreadGroup.waitN maxSize threadGroup
  ThreadGroup.forkOnWithUnmask i threadGroup f