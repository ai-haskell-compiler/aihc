{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Chan.Strict
-- Copyright   :  (c) The University of Glasgow 2001, Don Stewart 2007
-- License     :  BSD-style
-- 
-- Maintainer  :  dons@galois.com
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Unbounded, element-strict channels. Elements will be evaluated to
-- WHNF on entering the channel. For some concurrency applications, this
-- is more desirable than passing an unevaluted thunk through the channel
-- (for instance, it guarantees the node willl be evaluated to WHNF in a
-- worker thead).
--
-- Element-strict channes may potentially use more memory than lazy
-- channels
--
-----------------------------------------------------------------------------

module Control.Concurrent.Chan.Strict (
          -- * The 'Chan' type
        Chan,                   -- abstract

          -- * Operations
        newChan,                -- :: IO (Chan a)
        writeChan,              -- :: Chan a -> a -> IO ()
        readChan,               -- :: Chan a -> IO a
        dupChan,                -- :: Chan a -> IO (Chan a)
        unGetChan,              -- :: Chan a -> a -> IO ()
        isEmptyChan,            -- :: Chan a -> IO Bool

          -- * Stream interface
        getChanContents,        -- :: Chan a -> IO [a]
        writeList2Chan,         -- :: Chan a -> [a] -> IO ()
   ) where

import Prelude

import System.IO.Unsafe         ( unsafeInterleaveIO )
import Control.Concurrent.MVar.Strict
import Control.DeepSeq

-- A channel is represented by two @MVar@s keeping track of the two ends
-- of the channel contents,i.e.,  the read- and write ends. Empty @MVar@s
-- are used to handle consumers trying to read from an empty channel.

-- |'Chan' is an abstract type representing an unbounded FIFO channel.
data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))

type Stream a = MVar (ChItem a)

data ChItem a = ChItem !a (Stream a)

instance NFData a => NFData (ChItem a) where
    rnf (ChItem a s) = rnf a `seq` rnf s

-- @newChan@ sets up the read and write end of a channel by initialising
-- these two @MVar@s with an empty @MVar@.

-- |Build and returns a new instance of 'Chan'.
newChan :: NFData a => IO (Chan a)
newChan = do
   hole  <- newEmptyMVar
   readm <- newMVar hole
   write <- newMVar hole
   return (Chan readm write)

-- To put an element on a channel, a new hole at the write end is created.
-- What was previously the empty @MVar@ at the back of the channel is then
-- filled in with a new stream element holding the entered value and the
-- new hole.

-- |Write a value to a 'Chan'.
writeChan :: NFData a => Chan a -> a -> IO ()
writeChan (Chan _read write) val = do
  new_hole <- newEmptyMVar
  modifyMVar_ write $ \old_hole -> do
    putMVar old_hole $! ChItem val new_hole
    return new_hole

-- |Read the next value from the 'Chan'.
readChan :: NFData a => Chan a -> IO a
readChan (Chan readm _write) = do
  modifyMVar readm $ \read_end -> do
    (ChItem val new_read_end) <- readMVar read_end
        -- Use readMVar here, not takeMVar,
        -- else dupChan doesn't work
    return (new_read_end, val)

-- |Duplicate a 'Chan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both.  Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
dupChan :: NFData a => Chan a -> IO (Chan a)
dupChan (Chan _read write) = do
   hole     <- readMVar write
   new_read <- newMVar hole
   return (Chan new_read write)

-- |Put a data item back onto a channel, where it will be the next item read.
unGetChan :: NFData a => Chan a -> a -> IO ()
unGetChan (Chan readm _write) val = do
   new_read_end <- newEmptyMVar
   modifyMVar_ readm $ \read_end -> do
     putMVar new_read_end (ChItem val read_end)
     return new_read_end

-- |Returns 'True' if the supplied 'Chan' is empty.
isEmptyChan ::NFData a =>  Chan a -> IO Bool
isEmptyChan (Chan readm write) = do
   withMVar readm $ \r -> do
     w <- readMVar write
     let eq = r == w
     eq `seq` return eq

-- Operators for interfacing with functional streams.

-- |Return a lazy list representing the contents of the supplied
-- 'Chan', much like 'System.IO.hGetContents'.
getChanContents ::NFData a =>  Chan a -> IO [a]
getChanContents ch = unsafeInterleaveIO $ do
    x  <- readChan ch
    xs <- getChanContents ch
    return (x:xs)

-- |Write an entire list of items to a 'Chan'.
writeList2Chan ::NFData a =>  Chan a -> [a] -> IO ()
writeList2Chan = mapM_ . writeChan
