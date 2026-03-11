{-# LANGUAGE
        MultiParamTypeClasses,
        FunctionalDependencies,
        GADTs
  #-}

-- |This module defines the \"MRef\" abstraction, which is a set of
--  type-classes for things that behave like 'Control.Concurrent.MVar.MVar's.
--  See the documentation there for more info.  
--
--  This interface may be subject to future expansion.  Presently, rather 
--  than providing something like 'Control.Concurrent.MVar.tryTakeMVar',
--  instances for \"'Data.StateRef.Types.ReadRef' sr m ('Maybe' a)\" are
--  provided, giving 'Data.StateRef.Types.readReference' the same type 
--  tryTakeMRef would have if it existed.  There is currently nothing like
--  'Control.Concurrent.MVar.tryPutMVar', though.  Perhaps there should be.
--  Or, perhaps this is the sort of thing the weird (to me) signature of
--  'Data.IORef.atomicModifyIORef' is for, and an argument for a similar
--  signature for 'Data.StateRef.Types.modifyStateRef' or the addition of
--  a new atomicModifyStateRef function.
--
--  I would like to resolve these questions in version 0.3 of this package.
module Data.MRef.Types where

data MRef m a where
    MRef :: (TakeMRef sr m a, PutMRef sr m a) => !sr -> MRef m a

class HasMRef m where
    newMRef :: a -> m (MRef m a)
    newEmptyMRef :: m (MRef m a)

class Monad m => NewMRef sr m a | sr -> a where
    -- |See 'Control.Concurrent.MVar.newMVar'
    newMReference :: a -> m sr
    -- |See 'Control.Concurrent.MVar.newEmptyMVar'
    newEmptyMReference :: m sr
class Monad m => TakeMRef sr m a | sr -> a where
    -- |See 'Control.Concurrent.MVar.takeMVar'
    takeMReference :: sr -> m a
class Monad m => PutMRef sr m a | sr -> a where
    -- |See 'Control.Concurrent.MVar.putMVar'
    putMReference :: sr -> a -> m ()
