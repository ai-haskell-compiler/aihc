{-# LANGUAGE CPP #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Monad
import Data.Time.Clock

-- This is the same condition Delay.hs checks.  On Windows, and when -threaded is disabled,
-- we fall back to threads, which are much slower.
--
-- Moreover, when -threaded is disabled, timers seem to be less granular, so this test
-- uses a looser tolerance on timings.
hasFastTimers :: Bool
#if MIN_VERSION_base(4,4,0) && !mingw32_HOST_OS && !ghcjs_HOST_OS
hasFastTimers = rtsSupportsBoundThreads
#else
hasFastTimers = False
#endif

main :: IO ()
main = do
    trivial
    replicateConcurrently_ 10 trivial
    bench

trivial :: IO ()
trivial = do
    let new t = do
            delay <- newDelay t
            return (delay, atomically $ tryWaitDelay delay)

    -- The delay times out at the right time, and after tryWaitDelay returns
    -- 'True', 'updateDelay' and 'cancelDelay' have no observable effect.
    (delay, wait) <- new 100000
    False <- wait
    threadDelay 50000
    False <- wait
    threadDelay 60000
    True <- wait
    updateDelay delay 1000000
    True <- wait
    updateDelay delay (-1)
    True <- wait
    cancelDelay delay
    True <- wait

    (delay, wait) <- new 100000
    False <- wait               -- 100000us left
    threadDelay 50000
    False <- wait               -- 50000us left
    updateDelay delay 200000
    threadDelay 60000
    False <- wait               -- 140000us left
    threadDelay 60000
    False <- wait               -- 80000us left
        -- updateDelay sets the timer based on the current time,
        -- so the threadDelay 50000 doesn't count toward our total.

    -- In -threaded mode, expect a tighter tolerance for threadDelay timings.
    if hasFastTimers
        then threadDelay 81000      -- wait until 1000us after ring
        else threadDelay 150000     -- wait until 70000us after ring
    True <- wait
        -- We waited 201000 after setting the delay, so the delay must be expired now.
        -- The only way this could fail is if it takes more than a millisecond
        -- for updateDelay to take an MVar and write a TVar.  Context switching
        -- should not take this long.

    -- 'newDelay n' where n <= 0 times out immediately,
    -- rather than never timing out.
    (delay, wait) <- new 0
    threadDelay 100
    True <- wait
    (delay, wait) <- new (-1)
    threadDelay 100
    True <- wait

    -- This fails on Windows without -threaded, as 'threadDelay minBound'
    -- blocks.  It also fails on Linux using GHC 7.0.3 without -threaded.
#if !mingw32_HOST_OS && MIN_VERSION_base(4,4,0)
    (delay, wait) <- new minBound
    threadDelay 1000
    True <- wait
#endif

    -- 'newDelay maxBound' doesn't time out any time soon,
    -- and updateDelay doesn't wait for the delay to complete.
    --
    -- Using maxBound currently fails on Linux 64-bit (see GHC ticket #7325),
    -- so use a more lenient value for now.
    --
    -- (delay, wait) <- new maxBound
    (delay, wait) <- new 2147483647     -- 35 minutes, 47 seconds
    False <- wait
    threadDelay 100000
    False <- wait                       -- 35 minutes, 46.9 seconds left
    updateDelay delay 100000
    threadDelay 90000
    False <- wait                       -- 10000us left
    if hasFastTimers
        then threadDelay 10010 -- wait until 10us after ring
        else threadDelay 60000 -- wait until 50000us after ring
    True <- wait
        -- We waited 10 microseconds longer than the delay is for, so the delay
        -- must be expired now.  The only way this could fail is if it takes
        -- more than 10 microseconds for updateDelay to take an MVar and write a TVar.
        -- This might be conceivable with context switching.

    -- cancelDelay causes the delay to miss its initial deadline,
    -- and a subsequent updateDelay has no effect.
    (delay, wait) <- new 100000
    False <- wait
    threadDelay 50000
    False <- wait
    cancelDelay delay
    False <- wait
    threadDelay 60000
    False <- wait
    updateDelay delay 10000
    False <- wait
    threadDelay 20000
    False <- wait
    cancelDelay delay
    False <- wait
    threadDelay 100000
    False <- wait

    return ()

bench :: IO ()
bench = do
    startTime <- getCurrentTime

    let count = if hasFastTimers then 1000000 else 20000

    -- Create a bunch of timers of pseudorandom durations (under 2 seconds), and wait for all of them.
    delays <- mapM newDelay $ take count $ iterate (\n -> (n + 349000) `mod` 2000000) 0
    mapM_ (atomically . waitDelay) delays

    -- The operation should not take substantially more than 2 seconds.
    -- On an M4 MacBook this takes 2.5 to 2.6 seconds.
    endTime <- getCurrentTime
    let duration = endTime `diffUTCTime` startTime
    putStrLn $ "Creating and waiting for " ++ show count ++ " delays took " ++ show duration ++ "."
    when (duration > 4.0) $
        fail $ "newDelay and waitDelay are too slow"
