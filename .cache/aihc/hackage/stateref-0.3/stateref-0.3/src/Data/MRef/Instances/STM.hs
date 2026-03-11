{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

-- |This module exports no new symbols of its own.  It defines 
--  basic class instances for creating, reading, and writing 'TVar's and
--  (if available) 'TMVar's, and re-exports the types for which it defines 
--  instances as well as the 'atomically' function, which is indispensible
--  when playing with this stuff in ghci.
module Data.MRef.Instances.STM
    ( STM
#ifdef useTMVar
    , TMVar
#endif
    , TVar
    
    , atomically
    ) where

import Data.MRef.Types
import Data.StateRef (readReference, writeReference, newReference)
import Data.StateRef.Instances.STM ()

import Control.Concurrent.STM

-- MRef STM in IO monad
instance NewMRef (MRef STM a) IO a where
#ifdef useTMVar
    newMReference = fmap MRef . newTMVarIO
    newEmptyMReference = fmap MRef newEmptyTMVarIO
#else
    newMReference = fmap MRef . newTVarIO . Just
    newEmptyMReference = fmap MRef (newTVarIO Nothing)
#endif
    
instance TakeMRef (MRef STM a) IO a where
    takeMReference (MRef ref) = atomically (takeMReference ref)
instance PutMRef (MRef STM a) IO a where
    putMReference (MRef ref) = atomically . putMReference ref


#ifdef useTMVar
--TMVar in STM monad
instance HasMRef STM where
    newMRef x    = fmap MRef (newTMVar x)
    newEmptyMRef = fmap MRef newEmptyTMVar
instance NewMRef (TMVar a) STM a where
    newMReference = newTMVar
    newEmptyMReference = newEmptyTMVar

instance TakeMRef (TMVar a) STM a where
    takeMReference = takeTMVar
instance PutMRef (TMVar a) STM a where
    putMReference = putTMVar

-- TMVar in IO monad
instance NewMRef (TMVar a) IO a where
    newMReference = newTMVarIO
    newEmptyMReference = newEmptyTMVarIO
    
instance TakeMRef (TMVar a) IO a where
    takeMReference = atomically . takeMReference
instance PutMRef (TMVar a) IO a where
    putMReference ref = atomically . putMReference ref
#endif

-- incidental instances, which may occasionally be handy in a pinch
-- TVars containing "Maybe" values in STM monad.
-- Also use as default if TMVar isn't available.
#ifndef useTMVar
instance HasMRef STM where
    newMRef x    = fmap MRef (newTVar (Just x))
    newEmptyMRef = fmap MRef (newTVar Nothing)
#endif
instance NewMRef (TVar (Maybe a)) STM a where
    newMReference = newReference . Just
    newEmptyMReference = newReference Nothing

instance TakeMRef (TVar (Maybe a)) STM a where
    takeMReference ref = do
        x <- readReference ref
        case x of
            Nothing -> retry
            Just x -> do
                writeReference ref Nothing
                return x
instance PutMRef (TVar (Maybe a)) STM a where
    putMReference ref val = do
        x <- readReference ref
        case x of
            Nothing -> writeReference ref (Just val)
            Just x -> retry

-- TVars containing "Maybe" values in IO monad
instance NewMRef (TVar (Maybe a)) IO a where
    newMReference = newReference . Just
    newEmptyMReference = newReference Nothing
instance TakeMRef (TVar (Maybe a)) IO a where
    takeMReference = atomically . takeMReference
instance PutMRef (TVar (Maybe a)) IO a where
    putMReference ref = atomically . putMReference ref
