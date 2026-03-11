{-# LANGUAGE Safe #-}
module Control.Concurrent.PooledIO.Monad where

import qualified Control.Concurrent.Split.MVar as MVar
import Control.Concurrent (ThreadId, forkIO, getNumCapabilities, myThreadId, killThread)
import Control.DeepSeq (NFData, ($!!))
import Control.Exception (SomeException, finally, try, throw)

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.IO.Class (liftIO)

import Control.Monad (replicateM_, liftM2)
import Control.Functor.HT (void)

import qualified Data.Foldable as Fold
import qualified Data.Set as Set; import Data.Set (Set)


type T = MR.ReaderT (MVar.In (), MVar.Out ()) (MS.StateT Int IO)


fork :: (NFData a) => IO a -> T (IO a)
fork act = do
   (completeIn, completeOut) <- MR.ask
   initial <- MT.lift MS.get
   if initial>0
     then MT.lift $ MS.put (initial-1)
     else liftIO $ MVar.take completeOut
   liftIO $ do
      (resultIn, resultOut) <- MVar.newEmpty
      forkFinally completeIn $ (MVar.put resultIn $!!) =<< act
      return $ MVar.take resultOut

forkFinally :: MVar.In () -> IO () -> IO ()
forkFinally mvar act =
   void $ forkIO $ finally act $ MVar.put mvar ()

forkTry ::
   (NFData a) =>
   MVar.In (ThreadId, Either SomeException a) -> IO a ->
   MS.StateT (Set ThreadId) IO ()
forkTry mvar act = do
   thread <-
      liftIO $ forkIO $
         applyStrictRight (MVar.put mvar) =<< liftM2 (,) myThreadId (try act)
   MS.modify (Set.insert thread)

applyStrictRight :: (NFData a) => ((id, Either e a) -> b) -> (id, Either e a) -> b
applyStrictRight f (thread, ee) =
   case ee of
      Left e -> f (thread, Left e)
      Right a -> f . (,) thread . Right $!! a

takeMVarTry ::
   MVar.Out (ThreadId, Either SomeException a) ->
   MS.StateT (Set ThreadId) IO a
takeMVarTry mvar = do
   (thread, ee) <- liftIO $ MVar.take mvar
   MS.modify (Set.delete thread)
   case ee of
      Left e -> do liftIO . Fold.mapM_ killThread =<< MS.get; liftIO $ throw e
      Right a -> return a

runTry :: MS.StateT (Set ThreadId) IO a -> IO a
runTry act = MS.evalStateT act Set.empty


chooseNumCapabilities :: Maybe Int -> IO Int
chooseNumCapabilities = maybe getNumCapabilities return

withNumCapabilities :: (Int -> a -> IO b) -> a -> IO b
withNumCapabilities run acts = do
   numCaps <- getNumCapabilities
   run numCaps acts

runLimited :: Int -> T a -> IO a
runLimited maxThreads m = do
   complete@(_, completeOut) <- MVar.newEmpty
   (result, uninitialized) <-
      MS.runStateT (MR.runReaderT m complete) maxThreads
   replicateM_ (maxThreads-uninitialized) $ MVar.take completeOut
   return result
