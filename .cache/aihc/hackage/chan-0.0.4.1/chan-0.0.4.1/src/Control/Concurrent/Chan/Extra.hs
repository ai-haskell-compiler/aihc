{-# LANGUAGE
    DataKinds
  , KindSignatures
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , ScopedTypeVariables
  , PartialTypeSignatures
  #-}

module Control.Concurrent.Chan.Extra where

import Control.Concurrent.Chan.Typed
  ( ChanRW (..), writeChanRW, readChanRW, newChanRW)
import Control.Concurrent.STM.TChan.Typed
  ( TChanRW (..), writeTChanRW, readTChanRW, newTChanRW)
import Control.Concurrent.Chan.Scope (Scope (..), Writable, Readable)

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar, putMVar)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.STM
  (atomically, tryTakeTMVar, putTMVar, newEmptyTMVarIO)
import Control.Concurrent.STM.TChan (TChan)


-- | Class for changing the access of a typed channel
class ChanScoped (c :: Scope -> * -> *) where
  readOnly :: Readable scope => c scope a -> c 'Read a
  writeOnly :: Writable scope => c scope a -> c 'Write a
  allowReading :: Writable scope => c scope a -> c 'ReadWrite a
  allowWriting :: Readable scope => c scope a -> c 'ReadWrite a


type DiffNanosec = Int


-- | Class for extra channel techniques
class ChanExtra (inputC :: * -> *) (outputC :: * -> *)
      | inputC -> outputC, outputC -> inputC where
  -- | Throw away messages that meet the threshold
  debounceStatic :: DiffNanosec -- ^ Time to wait before attempting to send the message
                 -> outputC a -> IO (inputC a, Async ())
  -- | Refrain from relaying messages that meet the threshold
  throttleStatic :: DiffNanosec -- ^ Time to wait before sending the message, for every message
                 -> outputC a -> IO (inputC a, Async ())
  -- | Intercalate messages while threshold is met
  intersperseStatic :: DiffNanosec -- ^ Time to at-least wait to intersperse messages
                    -> IO a -- ^ Get a message to intersperse
                    -> outputC a
                    -> IO (inputC a, Async (), Async ())


instance ChanExtra Chan Chan where
  debounceStatic toWaitFurther outputChan = do
    (ChanRW inputChan, t) <- debounceStatic toWaitFurther (ChanRW outputChan :: ChanRW 'Read _)
    pure (inputChan, t)
  throttleStatic toWaitFurther outputChan = do
    (ChanRW inputChan, t) <- throttleStatic toWaitFurther (ChanRW outputChan :: ChanRW 'Read _)
    pure (inputChan, t)
  intersperseStatic timeBetween xM outputChan = do
    (ChanRW inputChan, writer, listener) <- intersperseStatic timeBetween xM (ChanRW outputChan :: ChanRW 'Read _)
    pure (inputChan, writer, listener)


instance ChanExtra TChan TChan where
  debounceStatic toWaitFurther outputTChan = do
    (TChanRW inputTChan, t) <- debounceStatic toWaitFurther (TChanRW outputTChan :: TChanRW 'Read _)
    pure (inputTChan, t)
  throttleStatic toWaitFurther outputTChan = do
    (TChanRW inputTChan, t) <- throttleStatic toWaitFurther (TChanRW outputTChan :: TChanRW 'Read _)
    pure (inputTChan, t)
  intersperseStatic timeBetween xM outputTChan = do
    (TChanRW inputTChan, writer, listener) <- intersperseStatic timeBetween xM (TChanRW outputTChan :: TChanRW 'Read _)
    pure (inputTChan, writer, listener)



instance ChanScoped TChanRW where
  readOnly (TChanRW x) = TChanRW x
  writeOnly (TChanRW x) = TChanRW x
  allowReading (TChanRW x) = TChanRW x
  allowWriting (TChanRW x) = TChanRW x

instance ChanExtra (TChanRW 'Write) (TChanRW 'Read) where
  debounceStatic toWaitFurther outputChan = do
    presentedChan <- atomically newTChanRW
    writingThread <- newEmptyTMVarIO

    writer <- async $ forever $ do
      x <- atomically (readTChanRW presentedChan)
      newWriter <- async $ do
        threadDelay toWaitFurther
        atomically (writeTChanRW (allowWriting outputChan) x)
      mInvoker <- atomically (tryTakeTMVar writingThread)
      case mInvoker of
        Nothing -> pure ()
        Just i -> cancel i -- kill the old without writing to output
      atomically (putTMVar writingThread newWriter)

    pure (writeOnly presentedChan, writer)

  throttleStatic toWaitFurther outputChan = do
    presentedChan <- atomically newTChanRW
    writingThread <- newEmptyTMVarIO

    writer <- async $ forever $ do
      x <- atomically (readTChanRW presentedChan)
      mInvoker <- atomically (tryTakeTMVar writingThread)
      case mInvoker of
        Nothing -> pure ()
        Just i -> wait i -- cascade invocations, waiting until output writes
      newWriter <- async $ do
        threadDelay toWaitFurther
        atomically (writeTChanRW (allowWriting outputChan) x)
      atomically (putTMVar writingThread newWriter)

    pure (writeOnly presentedChan, writer)

  intersperseStatic timeBetween xM outputChan = do
    presentedChan <- atomically newTChanRW
    writingThread <- newEmptyTMVarIO

    writer <- async $ forever $ do
      mInvoker <- atomically (tryTakeTMVar writingThread)
      case mInvoker of
        Nothing -> pure () -- continue immediately if nothing's waiting
        Just i -> wait i -- block until previous ping is sent
      newWriter <- async $ do
        threadDelay timeBetween
        x <- xM -- get new ping message
        atomically (writeTChanRW (allowWriting outputChan) x) -- send it
      atomically (putTMVar writingThread newWriter) -- register pinger

    listener <- async $ forever $ do
      y <- atomically (readTChanRW presentedChan)
      mInvoker <- atomically (tryTakeTMVar writingThread)
      case mInvoker of
        Nothing -> pure ()
        Just i -> cancel i -- kill pinger

      atomically (writeTChanRW (allowWriting outputChan) y) -- immediately send regular message

    pure (writeOnly presentedChan, writer, listener)


instance ChanScoped ChanRW where
  readOnly (ChanRW x) = ChanRW x
  writeOnly (ChanRW x) = ChanRW x
  allowReading (ChanRW x) = ChanRW x
  allowWriting (ChanRW x) = ChanRW x

instance ChanExtra (ChanRW 'Write) (ChanRW 'Read) where
  debounceStatic toWaitFurther outputChan = do
    presentedChan <- newChanRW
    writingThread <- newEmptyMVar

    writer <- async $ forever $ do
      x <- readChanRW presentedChan

      newWriter <- async $ do
        threadDelay toWaitFurther
        writeChanRW (allowWriting outputChan) x

      mInvoker <- tryTakeMVar writingThread
      case mInvoker of
        Nothing -> pure ()
        Just i -> cancel i
      putMVar writingThread newWriter

    pure (writeOnly presentedChan, writer)

  throttleStatic toWaitFurther outputChan = do
    presentedChan <- newChanRW
    writingThread <- newEmptyMVar

    writer <- async $ forever $ do
      x <- readChanRW presentedChan

      mInvoker <- tryTakeMVar writingThread
      case mInvoker of
        Nothing -> pure ()
        Just i -> wait i
      newWriter <- async $ do
        threadDelay toWaitFurther
        writeChanRW (allowWriting outputChan) x
      putMVar writingThread newWriter

    pure (writeOnly presentedChan, writer)

  intersperseStatic timeBetween xM outputChan = do
    presentedChan <- newChanRW
    writingThread <- newEmptyMVar

    writer <- async $ forever $ do
      mInvoker <- tryTakeMVar writingThread
      case mInvoker of
        Nothing -> pure ()
        Just i -> wait i
      newWriter <- async $ do
        threadDelay timeBetween
        x <- xM
        writeChanRW (allowWriting outputChan) x
      putMVar writingThread newWriter

    listener <- async $ forever $ do
      y <- readChanRW presentedChan

      mInvoker <- tryTakeMVar writingThread
      case mInvoker of
        Nothing -> pure ()
        Just i -> cancel i

      writeChanRW (allowWriting outputChan) y

    pure (writeOnly presentedChan, writer, listener)



-- debounceDynamic :: (NominalDiffTime -> NominalDiffTime) -> Chan a -> IO (Chan a, Async ())
-- debounceDynamic toWait outputChan = do
--   sinceRef <- newTimeSince
--   totalWaited <- newIORef 0
--   presentedChan <- newChan
--   writingThread <- newEmptyMVar

--   let invokeWrite x = do
--         putStrLn "Being run.."

--         waited <- readIORef totalWaited
--         let toWaitFurther = toWait waited
--         writeIORef totalWaited (waited + toWaitFurther)

--         -- FIXME must use clocktime http://hackage.haskell.org/package/chan- overlayed invocations have
--         -- no concept of time spent, only have knoweldge of invocations
--         -- made

--         threadDelay toWaitFurther
--         putStrLn "waited done"
--         writeChan outputChan x

--   writer <- async $ forever $ do
--     x <- readChan presentedChan

--     newWriter <- async (invokeWrite x)

--     mInvoker <- tryTakeMVar writingThread
--     case mInvoker of
--       Nothing -> pure ()
--       Just i -> cancel i
--     print "killed"
--     putMVar writingThread newWriter

--   pure (presentedChan, writer)
