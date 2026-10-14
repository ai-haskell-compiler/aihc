module Aihc.Cli.Concurrent
  ( Result,
    newResult,
    publishResult,
    awaitResult,
    inspectResult,
    releaseResult,
    Handoff,
    newHandoff,
    sendValue,
    receiveValue,
    mapConcurrently,
    mapConcurrently_,
    forConcurrently,
    withWorker,
    PermitPool,
    newPermitPool,
    withPermit,
  )
where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar, tryReadMVar)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, bracket, bracket_, finally, mask, onException, throwIO, try)
import Control.Monad (forM, forM_, join, void)

-- | A producer publishes one value. Readers share the value until release.
newtype Result a = Result (MVar (Maybe a))

newResult :: IO (Result a)
newResult = Result <$> newEmptyMVar

publishResult :: Result a -> a -> IO ()
publishResult (Result cell) value = do
  published <- tryPutMVar cell (Just value)
  if published then pure () else ioError (userError "Result was already published")

awaitResult :: Result a -> IO a
awaitResult (Result cell) = readMVar cell >>= maybe (ioError (userError "Result was released")) pure

inspectResult :: Result a -> IO (Maybe a)
inspectResult (Result cell) = join <$> tryReadMVar cell

releaseResult :: Result a -> IO ()
releaseResult (Result cell) = modifyMVar_ cell (const (pure Nothing))

-- | A producer transfers one value to one consumer.
newtype Handoff a = Handoff (MVar a)

newHandoff :: IO (Handoff a)
newHandoff = Handoff <$> newEmptyMVar

sendValue :: Handoff a -> a -> IO ()
sendValue (Handoff cell) value = do
  sent <- tryPutMVar cell value
  if sent then pure () else ioError (userError "Handoff already contains a value")

receiveValue :: Handoff a -> IO a
receiveValue (Handoff cell) = takeMVar cell

-- | The owner cancels each worker and waits for its exit.
data Worker = Worker !ThreadId !(MVar ())

startWorker :: IO () -> IO Worker
startWorker action = do
  done <- newEmptyMVar
  thread <- forkIOWithUnmask (\unmask -> void (try (unmask action) :: IO (Either SomeException ())) `finally` putMVar done ())
  pure (Worker thread done)

stopWorker :: Worker -> IO ()
stopWorker (Worker thread done) = killThread thread >> readMVar done

withWorker :: IO () -> (Worker -> IO a) -> IO a
withWorker action = bracket (startWorker action) stopWorker

-- | Return results in input order. A worker exception cancels the group.
mapConcurrently :: (a -> IO b) -> [a] -> IO [b]
mapConcurrently action inputs = mask $ \restore -> do
  events <- newChan
  let start input = do
        result <- newEmptyMVar
        worker <- startWorker $ do
          outcome <- try (action input)
          putMVar result outcome
          writeChan events outcome
        pure (worker, result)
      stop = mapM_ (stopWorker . fst)
      startAll [] = pure []
      startAll (input : rest) = do
        item <- start input
        items <- startAll rest `onException` stop [item]
        pure (item : items)
  workers <- startAll inputs
  restore
    ( do
        forM_ inputs $ \_ -> readChan events >>= either (throwIO :: SomeException -> IO ()) (const (pure ()))
        forM workers $ \(_, result) -> readMVar result >>= either throwIO pure
    )
    `finally` stop workers

mapConcurrently_ :: (a -> IO b) -> [a] -> IO ()
mapConcurrently_ action = void . mapConcurrently action

forConcurrently :: [a] -> (a -> IO b) -> IO [b]
forConcurrently = flip mapConcurrently

-- | A permit limits concurrent access to a resource.
newtype PermitPool = PermitPool QSem

newPermitPool :: Int -> IO PermitPool
newPermitPool count = PermitPool <$> newQSem (max 1 count)

withPermit :: PermitPool -> IO a -> IO a
withPermit (PermitPool semaphore) = bracket_ (waitQSem semaphore) (signalQSem semaphore)
