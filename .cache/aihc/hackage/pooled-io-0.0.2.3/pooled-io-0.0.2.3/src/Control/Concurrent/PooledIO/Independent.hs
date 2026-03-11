{-# LANGUAGE Safe #-}
module Control.Concurrent.PooledIO.Independent (
   run,
   runLimited,
   runUnlimited,
   runException,
   ) where

import Control.Concurrent.PooledIO.Monad
          (withNumCapabilities, chooseNumCapabilities,
           forkFinally, forkTry, takeMVarTry, runTry)
import qualified Control.Concurrent.Split.MVar as MVar
import Control.Exception (evaluate)

import Control.Monad (replicateM_)


{- |
Execute all actions parallelly
but run at most @numCapabilities@ threads at once.
Stop when all actions are finished.
If a thread throws an exception this terminates only the throwing thread.
-}
run :: [IO ()] -> IO ()
run = withNumCapabilities runLimited

runLimited :: Int -> [IO ()] -> IO ()
runLimited numCaps acts = do
   let (start, queue) = splitAt numCaps acts
   n <- evaluate $ length start
   (mvarIn, mvarOut) <- MVar.newEmpty
   mapM_ (forkFinally mvarIn) start
   mapM_ (\act -> MVar.take mvarOut >> forkFinally mvarIn act) queue
   replicateM_ n $ MVar.take mvarOut

{- |
Execute all actions parallelly without a bound an the number of threads.
Stop when all actions are finished.
-}
runUnlimited :: [IO ()] -> IO ()
runUnlimited acts =
   mapM_ MVar.take =<< mapM fork acts

fork :: IO () -> IO (MVar.Out ())
fork act = do
   (mvarIn, mvarOut) <- MVar.newEmpty
   forkFinally mvarIn act
   return mvarOut


{- |
If a thread ends with an exception,
then terminate all threads and forward that exception.
@runException Nothing@ chooses to use all capabilities,
whereas @runException (Just n)@ chooses @n@ capabilities.
-}
runException :: Maybe Int -> [IO ()] -> IO ()
runException maybeNumCaps acts = do
   numCaps <- chooseNumCapabilities maybeNumCaps
   runOneBreaksAll numCaps acts

runOneBreaksAll :: Int -> [IO ()] -> IO ()
runOneBreaksAll numCaps acts = do
   let (start, queue) = splitAt numCaps acts
   n <- evaluate $ length start
   (mvarIn, mvarOut) <- MVar.newEmpty
   runTry $ do
      mapM_ (forkTry mvarIn) start
      mapM_ (\act -> takeMVarTry mvarOut >> forkTry mvarIn act) queue
      replicateM_ n $ takeMVarTry mvarOut
