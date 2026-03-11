{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

-- |
-- Module      :  Test.ChasingBottoms.TimeOut
-- Copyright   :  (c) Nils Anders Danielsson 2004-2022, 2024-2025
-- License     :  See the file LICENCE.
--
-- Maintainer  :  http://www.cse.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (preemptive scheduling)
--
-- When dealing with \"hard bottoms\", i.e. non-terminating
-- computations that do not result in exceptions, the following functions
-- may be handy.
--
-- Note that a computation is considered to have terminated when it
-- has reached weak head normal form (i.e. something distinct from
-- bottom).

module Test.ChasingBottoms.TimeOut
  ( Result(..)
  , timeOut
  , timeOut'
  , timeOutMicro
  , timeOutMicro'
  ) where

import Control.Concurrent
import Data.Dynamic
import qualified Control.Exception as E

import {-# SOURCE #-} qualified Test.ChasingBottoms.IsBottom as B

data Result a
  = Value a
  | NonTermination
  | Exception E.SomeException
    deriving (Show, Typeable)

-- | @'timeOut' n c@ runs @c@ for at most @n@ seconds (modulo
-- scheduling issues).
--
--   * If the computation terminates before that, then @'Value' v@ is
--     returned, where @v@ is the resulting value. Note that this
--     value may be equal to bottom, e.g. if @c = 'return'
--     'B.bottom'@.
--
--   * If the computation does not terminate, then 'NonTermination' is
--     returned.
--
--   * If the computation raises an exception, then @'Exception' e@ is
--     returned, where @e@ is the exception.
--
-- Note that a user-defined exception is used to terminate the
-- computation, so if @c@ catches all exceptions, or blocks
-- asynchronous exceptions, then 'timeOut' may fail to function
-- properly.

timeOut :: Int -> IO a -> IO (Result a)
timeOut = timeOutMicro . (* 10^6)

-- | 'timeOutMicro' takes a delay in microseconds. Note that the
-- resolution is not necessarily very high (the last time I checked it
-- was 0.02 seconds when using the standard runtime system settings
-- for GHC).

timeOutMicro :: Int -> IO a -> IO (Result a)
timeOutMicro delay io = do
  mv <- newEmptyMVar
  let putException = putMVar mv . Exception
  ioThread <- forkIO $
    (io >>= putMVar mv . Value)
    `E.catch` (\(e :: E.SomeException) ->
                case E.fromException e of
                  Just Die -> return ()  -- Thread properly killed.
                  Nothing  -> putException e)
  reaper <- forkIO $ do
    threadDelay delay
    putMVar mv NonTermination
  result <- takeMVar mv
  killThread' ioThread
  killThread reaper
  return result

-- Since 'ioThread' above should return exceptions raised in the code
-- it seems like a bad idea to kill the thread using killThread, which
-- raises @'AsyncException' 'ThreadKilled'@. We use the locally
-- defined type 'Die' instead.

data Die = Die deriving (Show, Typeable)

instance E.Exception Die

killThread' threadId = E.throwTo threadId Die

-- | 'timeOut'' is a variant which can be used for pure
-- computations. The definition,
--
-- @
--   'timeOut'' n = 'timeOut' n . 'E.evaluate'
-- @
--
-- ensures that @'timeOut'' 1 'B.bottom'@ usually returns @'Exception'
-- \<something\>@. (@'timeOut' 1 ('return' 'B.bottom')@ usually
-- returns @'Value' 'B.bottom'@; in other words, the computation
-- reaches whnf almost immediately, defeating the purpose of the
-- time-out.)

timeOut' :: Int -> a -> IO (Result a)
timeOut' n = timeOut n . E.evaluate

-- | 'timeOutMicro'' is the equivalent variant of 'timeOutMicro':
--
-- @
--  'timeOutMicro'' n = 'timeOutMicro' n . 'E.evaluate'
-- @

timeOutMicro' :: Int -> a -> IO (Result a)
timeOutMicro' n = timeOutMicro n . E.evaluate

------------------------------------------------------------------------

-- There shouldn't be any memory leaks in the code above. Profiling
-- the code below also seems to suggest that there aren't any
-- problems. However, GHCi (with :set +r) eats up more and more memory
-- if the computation below is rerun a couple of times. Hmm, that
-- seems to be the case also when running simply (reverse [1..]). It
-- probably means that GHCi never releases any memory.

main = do
  let n = 1; d = 000000
  {-# SCC "a" #-} timeOut' n (reverse [1..]) >>= print
  threadDelay d
  {-# SCC "b" #-} timeOut' n (reverse [1..]) >>= print
  threadDelay d
  {-# SCC "c" #-} timeOut' n (reverse [1..]) >>= print
  threadDelay d
  {-# SCC "d" #-} timeOut' n (reverse [1..]) >>= print
