{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- |
-- Module:      Control.Concurrent.STM.Delay
-- Copyright:   (c) Joseph Adams 2012
-- License:     BSD3
-- Maintainer:  joeyadams3.14159@gmail.com
-- Portability: Requires GHC 7+
--
-- One-shot timer whose duration can be updated.  Think of it as an enhanced
-- version of 'registerDelay'.
--
-- This uses "GHC.Event" when available (GHC 7.2+, @-threaded@, non-Windows OS).
-- Otherwise, it falls back to forked threads and 'threadDelay'.
module Control.Concurrent.STM.Delay (
    -- * Managing delays
    Delay,
    newDelay,
    updateDelay,
    cancelDelay,

    -- * Waiting for expiration
    waitDelay,
    tryWaitDelay,
    tryWaitDelayIO,

    -- * Example
    -- $example
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception        (mask_)
import Control.Monad

#if MIN_VERSION_base(4,4,0) && !mingw32_HOST_OS && !ghcjs_HOST_OS
import qualified GHC.Event as Ev
#endif

-- | A 'Delay' is an updatable timer that rings only once.
data Delay = Delay
    { delayVar    :: !(TVar Bool)
    , delayUpdate :: !(Int -> IO ())
    , delayCancel :: !(IO ())
    }

instance Eq Delay where
    (==) a b = delayVar a == delayVar b

-- | Create a new 'Delay' that will ring in the given number of microseconds.
newDelay :: Int -> IO Delay
newDelay t
  | t > 0 = getDelayImpl t

  -- Special case zero timeout, so user can create an
  -- already-rung 'Delay' efficiently.
  | otherwise = do
        var <- newTVarIO True
        return Delay
            { delayVar    = var
            , delayUpdate = \_t -> return ()
            , delayCancel = return ()
            }

-- | Set an existing 'Delay' to ring in the given number of microseconds
-- (from the time 'updateDelay' is called), rather than when it was going to
-- ring.  If the 'Delay' has already rung, do nothing.
updateDelay :: Delay -> Int -> IO ()
updateDelay = delayUpdate

-- | Set a 'Delay' so it will never ring, even if 'updateDelay' is used later.
-- If the 'Delay' has already rung, do nothing.
cancelDelay :: Delay -> IO ()
cancelDelay = delayCancel

-- | Block until the 'Delay' rings.  If the 'Delay' has already rung,
-- return immediately.
waitDelay :: Delay -> STM ()
waitDelay delay = do
    expired <- tryWaitDelay delay
    if expired then return ()
               else retry

-- | Non-blocking version of 'waitDelay'.
-- Return 'True' if the 'Delay' has rung.
tryWaitDelay :: Delay -> STM Bool
tryWaitDelay = readTVar . delayVar

-- | Faster version of @'atomically' . 'tryWaitDelay'@.  See 'readTVarIO'.
--
-- Since 0.1.1
tryWaitDelayIO :: Delay -> IO Bool
tryWaitDelayIO = readTVarIO . delayVar

------------------------------------------------------------------------
-- Drivers

getDelayImpl :: Int -> IO Delay
#if MIN_VERSION_base(4,7,0) && !mingw32_HOST_OS && !ghcjs_HOST_OS
getDelayImpl t0 = do
    m <- Ev.getSystemEventManager
    case m of
        Nothing -> implThread t0
        Just _ -> do
            mgr <- Ev.getSystemTimerManager
            implEvent mgr t0
#elif MIN_VERSION_base(4,4,0) && !mingw32_HOST_OS && !ghcjs_HOST_OS
getDelayImpl t0 = do
    m <- Ev.getSystemEventManager
    case m of
        Nothing  -> implThread t0
        Just mgr -> implEvent mgr t0
#else
getDelayImpl = implThread
#endif

#if MIN_VERSION_base(4,7,0) && !mingw32_HOST_OS && !ghcjs_HOST_OS
-- | Use the timeout API in "GHC.Event" via TimerManager
--implEvent :: Ev.TimerManager -> Int -> IO Delay
implEvent mgr t0 = do
    var <- newTVarIO False
    k <- Ev.registerTimeout mgr t0 $ atomically $ writeTVar var True
    return Delay
        { delayVar    = var
        , delayUpdate = Ev.updateTimeout mgr k
        , delayCancel = Ev.unregisterTimeout mgr k
        }
#elif MIN_VERSION_base(4,4,0) && !mingw32_HOST_OS && !ghcjs_HOST_OS
-- | Use the timeout API in "GHC.Event"
implEvent :: Ev.EventManager -> Int -> IO Delay
implEvent mgr t0 = do
    var <- newTVarIO False
    k <- Ev.registerTimeout mgr t0 $ atomically $ writeTVar var True
    return Delay
        { delayVar    = var
        , delayUpdate = Ev.updateTimeout mgr k
        , delayCancel = Ev.unregisterTimeout mgr k
        }
#endif

-- | Use threads and threadDelay:
--
--  [init]
--      Fork a thread to wait the given length of time, then set the TVar.
--
--  [delayUpdate]
--      Stop the existing thread and (unless the delay has been canceled)
--      fork a new thread.
--
--  [delayCancel]
--      Stop the existing thread, if any.
implThread :: Int -> IO Delay
implThread t0 = do
    var <- newTVarIO False
    let new t = forkTimeoutThread t $ atomically $ writeTVar var True
    mv <- new t0 >>= newMVar . Just
    return Delay
        { delayVar    = var
        , delayUpdate = replaceThread mv . fmap Just . new
        , delayCancel = replaceThread mv $ return Nothing
        }

replaceThread :: MVar (Maybe TimeoutThread)
              -> IO (Maybe TimeoutThread)
              -> IO ()
replaceThread mv new =
  join $ mask_ $ do
    m <- takeMVar mv
    case m of
        Nothing -> do
            -- Don't create a new timer thread after the 'Delay' has
            -- been canceled.  Otherwise, the behavior is inconsistent
            -- with GHC.Event.
            putMVar mv Nothing
            return (return ())
        Just tt -> do
            m' <- stopTimeoutThread tt
            case m' of
                Nothing -> do
                    -- Timer already rang (or will ring very soon).
                    -- Don't start a new timer thread, as it would
                    -- waste resources and have no externally
                    -- observable effect.
                    putMVar mv Nothing
                    return $ return ()
                Just kill -> do
                    new >>= putMVar mv
                    return kill

------------------------------------------------------------------------
-- TimeoutThread

data TimeoutThread = TimeoutThread !ThreadId !(MVar ())

-- | Fork a thread to perform an action after the given number of
-- microseconds.
--
-- 'forkTimeoutThread' is non-interruptible.
forkTimeoutThread :: Int -> IO () -> IO TimeoutThread
forkTimeoutThread t io = do
    mv <- newMVar ()
    tid <- compat_forkIOUnmasked $ do
        threadDelay t
        m <- tryTakeMVar mv
        -- If m is Just, this thread will not be interrupted,
        -- so no need for a 'mask' between the tryTakeMVar and the action.
        case m of
            Nothing -> return ()
            Just _  -> io
    return (TimeoutThread tid mv)

-- | Prevent the 'TimeoutThread' from performing its action.  If it's too late,
-- return 'Nothing'.  Otherwise, return an action (namely, 'killThread') for
-- cleaning up the underlying thread.
--
-- 'stopTimeoutThread' has a nice property: it is /non-interruptible/.
-- This means that, in an exception 'mask', it will not poll for exceptions.
-- See "Control.Exception" for more info.
--
-- However, the action returned by 'stopTimeoutThread' /does/ poll for
-- exceptions.  That's why 'stopTimeoutThread' returns this action rather than
-- simply doing it.  This lets the caller do it outside of a critical section.
stopTimeoutThread :: TimeoutThread -> IO (Maybe (IO ()))
stopTimeoutThread (TimeoutThread tid mv) =
    maybe Nothing (\_ -> Just (killThread tid)) `fmap` tryTakeMVar mv

------------------------------------------------------------------------
-- Compatibility

compat_forkIOUnmasked :: IO () -> IO ThreadId
#if MIN_VERSION_base(4,4,0)
compat_forkIOUnmasked io = forkIOWithUnmask (\_ -> io)
#else
compat_forkIOUnmasked = forkIOUnmasked
#endif

------------------------------------------------------------------------

{- $example
Suppose we are managing a network connection, and want to time it out if no
messages are received in over five minutes.  We'll create a 'Delay', and an
action to \"bump\" it:

@
  let timeoutInterval = 5 * 60 * 1000000 :: 'Int'
  delay <- 'newDelay' timeoutInterval
  let bump = 'updateDelay' delay timeoutInterval
@

This way, the 'Delay' will ring if it is not bumped for longer than
five minutes.

Now we fork the receiver thread:

@
  dead <- 'newEmptyTMVarIO'
  _ <- 'forkIO' $
    ('forever' $ do
         msg <- recvMessage
         bump
         handleMessage msg
     ) \`finally\` 'atomically' ('putTMVar' dead ())
@

Finally, we wait for the delay to ring, or for the receiver thread to fail due
to an exception:

@
  'atomically' $ 'waitDelay' delay \`orElse\` 'readTMVar' dead
@

Warning:

 * If /handleMessage/ blocks, the 'Delay' may ring due to @handleMessage@
   taking too long, rather than just @recvMessage@ taking too long.

 * The loop will continue to run until you do something to stop it.

It might be simpler to use "System.Timeout" instead:

@
  m <- 'System.Timeout.timeout' timeoutInterval recvMessage
  case m of
      Nothing  -> 'fail' \"timed out\"
      Just msg -> handleMessage msg
@

However, using a 'Delay' has the following advantages:

 * If @recvMessage@ makes a blocking FFI call (e.g. network I/O on Windows),
   'System.Timeout.timeout' won't work, since it uses an asynchronous
   exception, and FFI calls can't be interrupted with async exceptions.
   The 'Delay' approach lets you handle the timeout in another thread,
   while the FFI call is still blocked.

 * 'updateDelay' is more efficient than 'System.Timeout.timeout' when
   "GHC.Event" is available.
-}
