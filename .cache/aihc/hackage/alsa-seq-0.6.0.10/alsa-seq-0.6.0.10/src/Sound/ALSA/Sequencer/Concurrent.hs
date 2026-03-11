module Sound.ALSA.Sequencer.Concurrent
  ( threadWaitInput
  , threadWaitOutput
  , threadWaitDuplex

  , input
  , output
  , drainOutput
  ) where

import qualified Sound.ALSA.Sequencer.Poll as AlsaPoll
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Event as Event
import Sound.ALSA.Exception (code, )

import qualified Control.Exception as Exc
import qualified System.Posix.Poll as Poll
import qualified Data.EnumBitSet as EnumSet
import Control.Concurrent (yield, threadWaitRead, threadWaitWrite, forkIO, killThread, )
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, )
import Control.Exception (catchJust, )
import Control.Monad (guard, when, )
import Data.Function (fix, )
import Data.Word (Word, )
import Foreign.C.Error (eINTR, )
import System.IO.Error (isFullError, )
import System.Posix.Types (Fd, )


data WaitFd
  = WaitRead Fd
  | WaitWrite Fd

pollWaits :: Poll.Fd -> [WaitFd]
pollWaits (Poll.Fd f e _) =
  (if EnumSet.subset Poll.inp e then [WaitRead  f] else []) ++
  (if EnumSet.subset Poll.out e then [WaitWrite f] else [])

-- | Wait for any of the given events, like poll, and return the one that is ready
threadWaitPolls :: [WaitFd] -> IO WaitFd
threadWaitPolls [] = yield >> return undefined
threadWaitPolls [p@(WaitRead f)] = threadWaitRead f >> return p
threadWaitPolls [p@(WaitWrite f)] = threadWaitWrite f >> return p
threadWaitPolls l = do
  w <- newEmptyMVar
  let poll1 p =
         Exc.catch
            (threadWaitPolls [p] >>= putMVar w . Right)
            (putMVar w . Left)
  t <- mapM (forkIO . poll1) l
  r <- takeMVar w
  mapM_ killThread t
  either ioError return r

threadWaitEvents :: Poll.Events -> Seq.T mode -> IO ()
threadWaitEvents e sh =
  AlsaPoll.descriptors sh e >>=
  threadWaitPolls . concatMap pollWaits >>
  return ()

-- | Wait for new input to be available from the sequencer (even if there is already input in the buffer)
threadWaitInput :: Seq.AllowInput mode => Seq.T mode -> IO ()
threadWaitInput = threadWaitEvents Poll.inp

-- | Wait until new output may be drained from the buffer to the sequencer (even if the output buffer is already empty)
threadWaitOutput :: Seq.AllowOutput mode => Seq.T mode -> IO ()
threadWaitOutput = threadWaitEvents Poll.out

-- | Wait until new input is available or new output may be drained
threadWaitDuplex :: (Seq.AllowInput mode, Seq.AllowOutput mode) => Seq.T mode -> IO ()
threadWaitDuplex = threadWaitEvents (Poll.inp EnumSet..|. Poll.out)

catchFull :: IO a -> IO a -> IO a
catchFull f e = catchJust (guard . isFullError) f (\() -> e)

catchIntr :: IO a -> IO a
catchIntr f = catchJust (guard . (eINTR ==) . code) f (\() -> catchIntr f)

-- | A thread-compatible version of @Sound.ALSA.Sequencer.Event.input@.
-- This call is always blocking (unless there are already event in the input
-- buffer) but will not block other threads.  The sequencer, however, must be
-- set non-blocking or this will not work as expected.
input :: Seq.AllowInput mode => Seq.T mode -> IO Event.T
input sh = do
  n <- catchIntr $ Event.inputPending sh True
  when (n == 0) $ threadWaitInput sh
  fix $ catchFull (Event.input sh) . (threadWaitInput sh >>)

-- | A thread-compatible version of @Sound.ALSA.Sequencer.Event.output@.
-- This call is always blocking (unless there is space in the output
-- buffer) but will not block other threads.  The sequencer, however, must be
-- set non-blocking or this will not work as expected.
output :: Seq.AllowOutput mode => Seq.T mode -> Event.T -> IO Word
output sh ev =
  Event.outputBuffer sh ev `catchFull` do
    threadWaitOutput sh
    _ <- Event.drainOutput sh `catchFull` return (-1)
    output sh ev

-- | A thread-compatible version of @Sound.ALSA.Sequencer.Event.drainBuffer@.
-- This call is always blocking but will not block other threads.  The
-- sequencer, however, must be set non-blocking or this will not work as
-- expected.
drainOutput :: Seq.AllowOutput mode => Seq.T mode -> IO ()
drainOutput sh = do
  n <- Event.drainOutput sh `catchFull` return (-1)
  when (n /= 0) $ do
    threadWaitOutput sh
    drainOutput sh
