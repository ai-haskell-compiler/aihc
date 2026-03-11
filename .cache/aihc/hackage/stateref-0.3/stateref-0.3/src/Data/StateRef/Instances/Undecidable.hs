{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances,
        UndecidableInstances
  #-}

module Data.StateRef.Instances.Undecidable where

import Data.StateRef.Types

-- |Wrap a state reference that supports reading and writing, and add a
-- potentially thread-unsafe 'ModifyRef' instance.
newtype UnsafeModifyRef sr = UnsafeModifyRef sr

instance ReadRef sr m a => ReadRef (UnsafeModifyRef sr) m a where
    readReference (UnsafeModifyRef sr) = readReference sr
instance WriteRef sr m a => WriteRef (UnsafeModifyRef sr) m a where
    writeReference (UnsafeModifyRef sr) = writeReference sr
instance (Monad m, ReadRef sr m a, WriteRef sr m a) => ModifyRef (UnsafeModifyRef sr) m a where
    atomicModifyReference   = defaultAtomicModifyReference
    modifyReference         = defaultModifyReference

