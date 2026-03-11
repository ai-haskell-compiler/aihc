-- | The simplest input: keypresses. Nice for testing.

module Midair.GetChar (
     runGetChar
   , GetChar(..)
   ) where

import Midair.Core
import Midair.Handy

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM
import Control.Monad
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin)


data GetChar = GetChar Char
 deriving (Show, Read, Eq, Ord)

runGetChar :: SFlow GetChar (Fx a) -> IO (ThreadId, ThreadId, SFNodeRef GetChar (Fx a))
runGetChar startGraph = do
   hSetBuffering stdin NoBuffering

   getCharTChan <- newTChanIO
   getCharTid <- forkIO $ forever $ do
      c <- getChar
      atomically $ writeTChan getCharTChan $ GetChar c

   wholeGraphRef <- mkNodeRef startGraph

   graphTVar <- newTVarIO $ nRef wholeGraphRef
   tid <- forkIO . forever . (runFx =<<) . atomically $
      readTChan getCharTChan >>= \msg -> do
         fireGraph graphTVar msg

   -- It's a good idea to return a ref of the whole graph so in
   -- case the user didn't make the node refs they ended up
   -- wanting, they can at least swap the whole thing out:
   return (getCharTid, tid, wholeGraphRef)
