{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

-- |This module exports no new symbols of its own.  It defines 
--  basic class instances for creating, reading, and writing 'MVar's, and
--  re-exports 'MVar'.
module Data.MRef.Instances
    ( MVar
    , MonadIO(..)

#ifdef useSTM
    , module Data.MRef.Instances.STM
#endif
    ) where

#ifdef useSTM
import Data.MRef.Instances.STM
#endif

import Data.MRef.Types

import Control.Concurrent.MVar
import Control.Monad.Trans

-- preferred instances
-- MVar in IO monad
instance HasMRef IO where
    newMRef x    = fmap MRef (newMVar x)
    newEmptyMRef = fmap MRef newEmptyMVar
instance MonadIO m => NewMRef (MVar a) m a where
    newMReference = liftIO . newMVar
    newEmptyMReference = liftIO newEmptyMVar
instance MonadIO m => TakeMRef (MVar a) m a where
    takeMReference = liftIO . takeMVar
instance MonadIO m => PutMRef (MVar a) m a where
    putMReference r = liftIO . putMVar r
