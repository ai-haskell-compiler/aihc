{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  , DeriveGeneric
  #-}

module Control.Concurrent.Async.Every where

import Data.Maybe (fromMaybe)
import Control.Monad (forever)
import Control.Exception (Exception, catch)
import Control.Concurrent (ThreadId, forkIO, threadDelay, throwTo)
import Control.Concurrent.Async (async, Async, cancelWith, asyncThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import GHC.Generics (Generic)



-- | Spawn a process forever
every :: Int -- ^ time difference in microseconds
      -> Maybe Int -- ^ initial delay before first invocation
      -> IO a
      -> IO ThreadId
every interDelay mInitDelay x = do
  let timeToDelay _ = interDelay
  threadDelay $ fromMaybe 0 mInitDelay
  everyFunc timeToDelay x


everyForking :: Int
             -> Maybe Int
             -> IO a
             -> IO (ThreadId, TChan (Async a))
everyForking interDelay mInitDelay x = do
  let timeToDelay _ = interDelay
  threadDelay $ fromMaybe 0 mInitDelay
  everyFuncForking timeToDelay x


everyFunc :: forall a
           . (Int -> Int) -- ^ function from total time spent, to the time to delay from /now/
          -> IO a
          -> IO ThreadId
everyFunc timeToDelay x = do
  totalTimeSpent <- newTVarIO 0
  forkIO $
    let thread = do
          x
          toGo <- atomically $ do
            soFar <- readTVar totalTimeSpent
            let toGo' = timeToDelay soFar
            writeTVar totalTimeSpent (soFar + toGo')
            pure toGo'
          threadDelay toGo
          thread `catch` resetter
        resetter :: EveryException -> IO ()
        resetter (EveryExceptionReset mThreadDelay) = do
          threadDelay $ fromMaybe 0 mThreadDelay
          thread `catch` resetter
    in  thread `catch` resetter


-- | A version of 'everyFunc' which forks /every/ time *coolshades*.
everyFuncForking :: forall a
                  . (Int -> Int)
                 -> IO a
                 -> IO (ThreadId, TChan (Async a))
everyFuncForking timeToDelay x = do
  forkedChan <- newTChanIO
  mainThread <- everyFunc timeToDelay $ do
    latestThread <- async x
    atomically $ writeTChan forkedChan latestThread
  pure (mainThread,forkedChan)


reset :: Maybe Int -> ThreadId -> IO ()
reset mDelay thread = throwTo thread (EveryExceptionReset mDelay)


data EveryException = EveryExceptionReset (Maybe Int)
  deriving (Show, Eq, Generic)

instance Exception EveryException
