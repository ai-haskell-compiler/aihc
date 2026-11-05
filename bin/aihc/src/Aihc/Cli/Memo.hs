-- | A table that runs the action of a key once, however many tasks ask
-- for it at the same time.
module Aihc.Cli.Memo
  ( newMemo,
  )
where

import Control.Concurrent.STM (TMVar, TVar, atomically, modifyTVar', newEmptyTMVar, newTVarIO, putTMVar, readTMVar, readTVar, writeTVar)
import Control.Exception (SomeAsyncException, SomeException, fromException, mask, throwIO, try)
import Control.Monad (join)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | The result that the owner of a key gives to the tasks that wait.
data MemoResult value
  = MemoDone (Either SomeException value)
  | -- | An asynchronous exception stopped the owner. It is not a result
    -- of the action, so a task that waits runs the action again.
    MemoInterrupted

-- | The first task to ask for a key runs the action; the others wait for
-- its result, and get its exception if it has one.
newMemo :: (Ord key) => IO (key -> IO value -> IO value)
newMemo = do
  table <- newTVarIO Map.empty
  pure (memoize table)

-- The owner masks from the claim of its slot to the put, so a slot that
-- is in the table always gets a result.
memoize :: (Ord key) => TVar (Map key (TMVar (MemoResult value))) -> key -> IO value -> IO value
memoize table key action =
  join $ mask $ \restore -> do
    (slot, owner) <- atomically $ do
      entries <- readTVar table
      case Map.lookup key entries of
        Just slot -> pure (slot, False)
        Nothing -> do
          slot <- newEmptyTMVar
          writeTVar table (Map.insert key slot entries)
          pure (slot, True)
    if owner
      then do
        result <- try (restore action)
        case result of
          Left exception
            | isAsynchronous exception -> do
                -- A sibling task failed and the graph cancels this one.
                -- A task that waits must not report the cancellation in
                -- place of that failure.
                atomically $ do
                  modifyTVar' table (Map.delete key)
                  putTMVar slot MemoInterrupted
                pure (throwIO exception)
          _ -> do
            atomically (putTMVar slot (MemoDone result))
            pure (either throwIO pure result)
      else pure $ do
        outcome <- atomically (readTMVar slot)
        case outcome of
          MemoDone result -> either throwIO pure result
          MemoInterrupted -> memoize table key action

isAsynchronous :: SomeException -> Bool
isAsynchronous exception =
  case fromException exception :: Maybe SomeAsyncException of
    Just _ -> True
    Nothing -> False
