{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE LambdaCase #-}

-- | A small library for Erlang-style actor semantics, for coordinating
--   concurrent processes and message passing

module Control.Concurrent.NanoErl (
     Process
   , spawn
   , Pid
   , (!)
   , receive
   -- , receiveMaybe
   , runNanoErl
   , kill

--   , link
--   , trapExit
   ) where

import Control.Concurrent (ThreadId, forkFinally, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import Control.Concurrent.STM
   (atomically, TVar, newTVarIO, modifyTVar', readTVar, retry)
-- import Control.Concurrent.STM.TChan -- Too slow for large # of processes
import Control.Monad (when)
import GHC.Conc (threadStatus, ThreadStatus(..))

-- This is safe -- we're only using it for "the unsafePerformIO hack" to get
-- a toplevel variable
-- (https://wiki.haskell.org/Top_level_mutable_state)
import System.IO.Unsafe (unsafePerformIO)

-- With "runNanoErl", the program won't terminate until our
-- process counter is 0:
processCounter :: TVar Int
{-# NOINLINE processCounter #-}
processCounter = unsafePerformIO $ newTVarIO 0

-- | Start up a concurrent process and get a reference to it
spawn :: Process message -> IO (Pid message)
spawn process = do
   atomically $ modifyTVar' processCounter (+1)
   pidMVar <- newEmptyMVar
   tid <- forkFinally (process =<< takeMVar pidMVar) $ \_ ->
      -- Decrement process count when the process finishes:
      atomically $ modifyTVar' processCounter (subtract 1)
   pid <- Pid <$> pure tid <*> newChan
   -- Give the process its own pid (for 'self'):
   putMVar pidMVar $ pid
   return pid

-- | Process ID: the way we refer to concurrent processes.
--   Send messages to them with '!'
data Pid message
   = Pid ThreadId (Chan message)
 deriving (Eq)

instance Show (Pid message) where
   show (Pid tid _) =
      "pid:" ++ filter (`elem`['0'..'9']) (show tid)

instance Ord (Pid message) where
   compare (Pid tid0 _) (Pid tid1 _) =
      compare tid0 tid1

{-
data ReceiveData message
   = UserMessage message
   | SystemMessage
-}

-- | Send a message to another process's \"mailbox.\"
--   The messages are handled with 'receive'
-- 
--   Like Erlang, we ignore any messages sent to processes that have died
(!) :: Pid message -> message -> IO ()
(Pid tid mailbox) ! msg = threadStatus tid >>= \case
   ThreadFinished -> return ()
   ThreadDied -> return ()
   _ -> writeChan mailbox msg

-- | Receive data from the process's \"mailbox\"
-- 
--   If there are no messages in the mailbox, the process waits
--   until there is one then handles it
-- 
--   Note at the moment we don't enforce in the type system that a process is
--   only reading its own mailbox, but that's the idiom in Erlang (1 mailbox
--   per process)
receive :: Pid message -> (message -> IO a) -> IO a
receive (Pid _ mailbox) action =
   action =<< readChan mailbox

-- | A process takes its own "Pid", to receive messages with,
--   and can do any IO actions
type Process message = Pid message -> IO ()

-- | Kill a process
kill :: Pid message -> IO ()
kill (Pid tid _) = killThread tid

-- | Put this at the toplevel of your program, e.g.
-- 
--   > main = runNanoErl $ ...
-- 
--   To ensure that your program doesn't exit before all its 
--   'spawn'ed processes have finished running
runNanoErl :: IO a -> IO a
runNanoErl mainF = do
   mainResult <- mainF
   -- Wait for all 'spawn'ed threads to finish before we finish:
   atomically $ do
      numAlive <- readTVar processCounter
      when (numAlive > 0) retry
   return mainResult
