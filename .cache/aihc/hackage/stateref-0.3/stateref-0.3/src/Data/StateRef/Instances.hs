{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

-- |This module exports no new symbols of its own.  It defines several 
--  basic class instances for creating, reading, and writing standard
--  reference types, and re-exports the types for which it defines instances.
--  
--  TODO: add millions of SPECIALIZE INSTANCE pragmas, for IO monad at
--  a minimum.
module Data.StateRef.Instances
    ( IORef
    , MVar
    , MonadIO(..)
    
    , STRef
    , ST
    , RealWorld
    
    , ForeignPtr
    
#ifdef useSTM
    , module Data.StateRef.Instances.STM
#endif
    
    , module Data.StateRef.Instances.Undecidable
    
    ) where

#ifdef useSTM
import Data.StateRef.Instances.STM
#endif

import Data.StateRef.Types
import Data.StateRef.Instances.Undecidable

import Data.IORef
import Control.Concurrent.MVar

import Control.Monad.Trans
import Control.Monad.ST
import Data.STRef

import qualified Control.Monad.ST.Lazy
import qualified Data.STRef.Lazy

import Foreign.Storable
import Foreign.ForeignPtr

-- @Ref m@ in @m@:
instance HasRef m => NewRef (Ref m a) m a where
    newReference = newRef
instance ReadRef (Ref m a) m a where
    readReference (Ref sr) = readReference sr
instance WriteRef (Ref m a) m a where
    writeReference (Ref sr) = writeReference sr
instance ModifyRef (Ref m a) m a where
    atomicModifyReference (Ref sr) = atomicModifyReference sr
    modifyReference (Ref sr) = modifyReference sr

-- m a in semi-arbitrary monad m
-- (cannot have instance Monad m => ReadRef (m a) m a, because this activates
-- functional dependencies that would overconstrain totally unrelated instances
-- because of the possibility of the future addition of, e.g., instance Monad TMVar)
instance Monad m => NewRef (IO a) m a where
    newReference ro = return (return ro)
instance MonadIO m => ReadRef (IO a) m a where
    readReference = liftIO

instance Monad m => NewRef (ST s a) m a where
    newReference ro = return (return ro)
instance ReadRef (ST s a) (ST s) a where
    readReference = id
instance MonadIO m => ReadRef (ST RealWorld a) m a where
    readReference = liftIO . stToIO

-- IORef in IO-compatible monads
instance HasRef IO where
    newRef x = do
        sr <- newIORef x
        return (Ref sr)
instance MonadIO m => NewRef (IORef a) m a where
    newReference = liftIO . newIORef
instance MonadIO m => ReadRef (IORef a) m a where
    readReference = liftIO . readIORef
instance MonadIO m => WriteRef (IORef a) m a where
    writeReference r = liftIO . writeIORef r
instance MonadIO m => ModifyRef (IORef a) m a where
    atomicModifyReference r = liftIO . atomicModifyIORef r
    modifyReference r = liftIO . modifyIORef r

-- @Ref IO@ in IO-compatible monads
--   (maybe...)
-- instance MonadIO m => NewRef (Ref IO a) m a where
--         newReference (Ref sr) = liftIO (newIORef sr)
-- instance MonadIO m => ReadRef (Ref IO a) m a where
--         readReference (Ref sr) = liftIO (readIORef sr)
-- instance MonadIO m => WriteRef (Ref IO a) m a where
--         writeReference (Ref sr) = liftIO . writeIORef sr
-- instance MonadIO m => ModifyRef (Ref IO a) m a where
--         atomicModifyReference (Ref sr) = liftIO . atomicModifyIORef sr
--         modifyReference (Ref sr) = liftIO . modifyIORef sr

-- (STRef s) in (ST s) monad
instance HasRef (ST s) where
    newRef x = do
        sr <- newSTRef x
        return (Ref sr)
instance NewRef (STRef s a) (ST s) a where
    newReference = newSTRef
instance ReadRef (STRef s a) (ST s) a where
    readReference = readSTRef
instance WriteRef (STRef s a) (ST s) a where
    writeReference = writeSTRef
instance ModifyRef (STRef s a) (ST s) a where
    atomicModifyReference   = defaultAtomicModifyReference
    modifyReference         = defaultModifyReference

-- (STRef RealWorld) in IO monad (not MonadIO instances, because the m
--  would overlap with (ST s) even though there's no instance MonadIO (ST a))
instance NewRef (STRef RealWorld a) IO a where
    newReference = stToIO . newReference
instance ReadRef (STRef RealWorld a) IO a where
    readReference = stToIO . readReference
instance WriteRef (STRef RealWorld a) IO a where
    writeReference r = stToIO . writeReference r
instance ModifyRef (STRef RealWorld a) IO a where
    modifyReference r       = stToIO . modifyReference r
    atomicModifyReference r = stToIO . atomicModifyReference r

-- (STRef s) in lazy (ST s) monad
instance HasRef (Control.Monad.ST.Lazy.ST s) where
    newRef x = do
        sr <- Data.STRef.Lazy.newSTRef x
        return (Ref sr)
instance NewRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a where
    newReference = Data.STRef.Lazy.newSTRef
instance ReadRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a where
    readReference = Data.STRef.Lazy.readSTRef
instance WriteRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a where
    writeReference = Data.STRef.Lazy.writeSTRef
instance ModifyRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a where
    atomicModifyReference   = defaultAtomicModifyReference
    modifyReference         = defaultModifyReference

-- MVar in IO-compatible monads (constructable but not usable as a "normal" state ref)
instance MonadIO m => NewRef (MVar a) m (Maybe a) where
    newReference Nothing = liftIO newEmptyMVar
    newReference (Just x) = liftIO (newMVar x)

-- ForeignPtrs, Ptrs, etc., in IO-compatible monads
instance (Storable a, MonadIO m) => NewRef (ForeignPtr a) m a where
    newReference val = liftIO $ do
        ptr <- mallocForeignPtr
        withForeignPtr ptr (\ptr -> poke ptr val)
        return ptr
instance (Storable a, MonadIO m) => ReadRef (ForeignPtr a) m a where
    readReference ptr = liftIO (withForeignPtr ptr peek)
instance (Storable a, MonadIO m) => WriteRef (ForeignPtr a) m a where
    writeReference ptr val = liftIO (withForeignPtr ptr (\ptr -> poke ptr val))
instance (Storable a, MonadIO m) => ModifyRef (ForeignPtr a) m a where
    atomicModifyReference   = defaultAtomicModifyReference
    modifyReference         = defaultModifyReference

-- this is an instance I would like to make, but it opens
-- a big can of worms... it requires incoherent instances, for one.
-- perhaps I ought to give up the abstractness of 'sr' in the class
-- definition; i don't know if that gets me anywhere though... 
--
-- note that as long as only these instances exist, there is no
-- actual overlap.  maybe it's not such a bad idea.  on the other
-- hand, a corresponding instance for Reader would be nice too, and
-- that would be a duplicate instance (because only the context would
-- differ).
--
-- instance (MonadState s1 m,
--           StateRef s2 m a)
--                 => StateRef (s1 -> s2) m a
--         where
--                 readReference f       = do
--                         s1 <- get
--                         readReference (f s1)
--                 writeReference f val  = do
--                         s1 <- get
--                         writeReference (f s1) val
--                 modifyReference f g = do
--                         s1 <- get
--                         modifyReference (f s1) g
