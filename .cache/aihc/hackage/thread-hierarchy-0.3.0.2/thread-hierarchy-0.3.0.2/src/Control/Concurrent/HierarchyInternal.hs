
{-|
Module      : Control.Concurrent.HierarchyInternal
Copyright   : (c) Naoto Shimazaki 2017-2020
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

Internal implementations of Control.Concurrent.Hierarchy
-}
module Control.Concurrent.HierarchyInternal where

import           Control.Concurrent          (ThreadId, forkIOWithUnmask,
                                              killThread, myThreadId)
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, putMVar,
                                              readMVar)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO,
                                              readTVarIO)
import           Control.Exception           (AsyncException (ThreadKilled),
                                              catch, finally, mask_)
import           Control.Monad.STM           (atomically)
import           Data.Foldable               (for_, traverse_)
import           Data.Map.Strict             (Map, delete, elems, empty, insert,
                                              keys)


{-|
    FinishMarker is created as empty MVar when a thread is created.
    It is automatically filled by () when the thread exits.
-}
newtype FinishMarker = FinishMarker (MVar ()) deriving (Eq)

{-|
    Mutable map containing thread ID and finish marker.
-}
newtype ThreadMap = ThreadMap (TVar (Map ThreadId FinishMarker))

{-|
    Create a new empty 'ThreadMap'.
-}
newThreadMap :: IO ThreadMap
newThreadMap = ThreadMap <$> newTVarIO empty

{-|
    Create a new thread and register it to given 'ThreadMap'.
-}
newChild
    :: ThreadMap            -- ^ ThreadMap where newly created thread will be registered.
    -> (ThreadMap -> IO ()) -- ^ Action executed within the new thread.
    -> IO ThreadId          -- ^ newChild returns ThreadId of created thread.
newChild brothers@(ThreadMap bMap) action = do
    finishMarker <- FinishMarker <$> newEmptyMVar
    children <- newThreadMap
    mask_ $ do
        child <- forkIOWithUnmask $ \unmask ->
            unmask (action children) `finally` cleanup finishMarker brothers children
        atomically $ modifyTVar' bMap (insert child finishMarker)
        return child

{-|
    Kill all thread registered in given 'ThreadMap'.
    This version is exposed as API.  This is not called from cleanup routine.
    Thus it doesn't ignore asynchronous exceptions.
-}
killThreadHierarchy
    :: ThreadMap    -- ^ ThreadMap containing threads to be killed
    -> IO ()
killThreadHierarchy (ThreadMap children) = do
    currentChildren <- readTVarIO children
    traverse_ killThread $ keys currentChildren
    remainingChildren <- readTVarIO children
    traverse_ (\(FinishMarker marker) -> readMVar marker) $ elems remainingChildren

{-|
    Kill all thread registered in given 'ThreadMap'.
    This internal version is only called from cleanup routine so
    this ignores ThreadKilled asynchronous exception.
-}
killThreadHierarchyInternal
    :: ThreadMap    -- ^ ThreadMap containing threads to be killed
    -> IO ()
killThreadHierarchyInternal (ThreadMap children) = do
    {-
        Here we are going to kill all child threads for cleanup.  Because we are already under the way
        to terminate current thread, we don't want to be interrupted this process by ThreadKilled
        asynchronous exceptions.
        Though this routine runs under masked condition, there are two operation where asynchronous
        exception can interrupts them.  Those are killThread and readMVar.
        We just catch the exception, ignore it, then reattempt interrupted operation.
    -}
    currentChildren <- readTVarIO children
    for_ (keys currentChildren) $ \child ->
        killThread child `catch` (\ThreadKilled -> killThread child)
    remainingChildren <- readTVarIO children
    for_ (elems remainingChildren) $ \(FinishMarker marker) ->
        readMVar marker `catch` (\ThreadKilled -> readMVar marker)

{-|
    Thread clean up routine automatically installed by newChild.
    It first killThreadHierarchy all its child threads and unregister itself.
    This function is not an API function but for internal use only.
-}
cleanup :: FinishMarker -> ThreadMap -> ThreadMap -> IO ()
cleanup (FinishMarker marker) (ThreadMap brotherMap) children = do
    killThreadHierarchyInternal children
    myThread <- myThreadId
    atomically $ modifyTVar' brotherMap (delete myThread)
    putMVar marker ()   -- mark finish.
