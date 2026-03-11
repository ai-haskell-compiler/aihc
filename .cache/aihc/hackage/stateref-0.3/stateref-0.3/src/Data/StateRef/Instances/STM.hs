{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances,
        IncoherentInstances
  #-}

-- |This module exports no new symbols of its own.  It defines 
--  basic class instances for creating, reading, and writing 'TVar's and
--  (if available) 'TMVar's, and re-exports the types for which it defines 
--  instances as well as the 'atomically' function, which is indispensible
--  when playing with this stuff in ghci.
--
--  Note that this module declares incoherent instances.  The universe should
--  refrain from imploding on itself as long as you don't define
--  \"instance MonadIO STM\".  However, hugs doesn't seem to support 
--  overlapping instances, so I may have to give up on the dream of MonadIO 
--  everywhere, or introduce some major conditional compilation stuff. (or
--  abandon hugs support)

module Data.StateRef.Instances.STM
    ( STM
    , TVar
#ifdef useTMVar
    , TMVar
#endif
    
    , atomically
    ) where

import Data.StateRef.Types
import Control.Monad.Trans
import Control.Concurrent.STM

-- (STM a) in STM and IO-compatible monads
instance ReadRef (STM a) STM a where
    readReference = id
instance MonadIO m => ReadRef (STM a) m a where
    readReference = liftIO . atomically

-- TVar in STM monad
instance HasRef STM where
    newRef x = do
        sr <- newTVar x
        return (Ref sr)
instance NewRef (TVar a) STM a where
    newReference = newTVar
instance ReadRef (TVar a) STM a where
    readReference = readTVar
instance WriteRef (TVar a) STM a where
    writeReference = writeTVar
instance ModifyRef (TVar a) STM a where
    atomicModifyReference   = defaultAtomicModifyReference
    modifyReference         = defaultModifyReference

-- TVar in IO-compatible monads
instance MonadIO m => NewRef (TVar a) m a where
    newReference = liftIO . newTVarIO
instance MonadIO m => ReadRef (TVar a) m a where
    readReference = liftIO . atomically . readReference
instance MonadIO m => WriteRef (TVar a) m a where
    writeReference ref = liftIO . atomically . writeReference ref
instance MonadIO m => ModifyRef (TVar a) m a where
    modifyReference ref         = liftIO . atomically . modifyReference ref
    atomicModifyReference ref   = liftIO . atomically . atomicModifyReference ref

-- @Ref STM@ in IO-compatible monads
instance MonadIO m => NewRef (Ref STM a) m a where
    newReference x = do
        sr <- liftIO (newTVarIO x)
        return (Ref sr)
instance MonadIO m => ReadRef (Ref STM a) m a where
    readReference (Ref sr) = liftIO (atomically (readReference sr))
instance MonadIO m => WriteRef (Ref STM a) m a where
    writeReference (Ref sr) = liftIO . atomically . writeReference sr
instance MonadIO m => ModifyRef (Ref STM a) m a where
    modifyReference (Ref sr)        = liftIO . atomically . modifyReference sr
    atomicModifyReference (Ref sr)  = liftIO . atomically . atomicModifyReference sr

#ifdef useTMVar
-- TMVar in STM monad
instance NewRef (TMVar a) STM (Maybe a) where
    newReference Nothing = newEmptyTMVar
    newReference (Just x) = newTMVar x
instance ReadRef (TMVar a) STM (Maybe a) where
    readReference tmv = fmap Just (readTMVar tmv) `orElse` return Nothing

-- TMVar in IO-compatible monad
instance MonadIO m => NewRef (TMVar a) m (Maybe a) where
    newReference Nothing = liftIO newEmptyTMVarIO
    newReference (Just x) = liftIO (newTMVarIO x)
instance MonadIO m => ReadRef (TMVar a) m (Maybe a) where
    readReference = liftIO . atomically . readReference
#endif
