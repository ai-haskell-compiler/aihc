{-# LANGUAGE 
    MultiParamTypeClasses,
    FunctionalDependencies,
    GADTs
  #-}

module Data.StateRef.Types where

-- |A simple reference type, hiding the complexity of all these type classes,
-- since most of the time the only thing you care about is that you want a reference.
-- The full complexity is still there, though, so FFI types or other reference-like
-- things can still be made into 'Ref's.
data Ref m a where
    Ref :: ModifyRef sr m a => !sr -> Ref m a

class WriteRef sr m a | sr -> a where
    -- |Replace the existing value of the given reference with the provided value.
    writeReference :: sr -> a -> m ()

class ReadRef sr m a | sr -> a where
    -- |Get the current value referenced by the given state reference.
    readReference :: sr -> m a

class (ReadRef sr m a, WriteRef sr m a) => ModifyRef sr m a | sr -> a where
    -- |Atomically modify the contents of a reference.  This is
    -- implemented in a separate class (rather than a function with
    -- context (ReadRef sr m a, WriteRef sr m a)) because in most
    -- cases the default implementation cannot act atomically.
    atomicModifyReference :: sr -> (a -> (a,b)) -> m b
    
    -- |Same thing, but don't thread out the extra return.  Could perhaps
    -- be implemented slightly more efficiently than 'atomicModifyReference' in many cases.
    -- Note that implementations are expected to be atomic, if at all possible,
    -- but not strictly required to be.
    modifyReference :: sr -> (a -> a) -> m ()

-- |Default implementation of atomicModifyReference in terms of readReference and writeReference
defaultAtomicModifyReference ref f = do
    x <- readReference ref
    let (x', b) = f x
    writeReference ref x'
    return b

-- |Default implementation of modifyReference in terms of readReference and writeReference
defaultModifyReference ref f = do
    x <- readReference ref
    let x' = f x
    writeReference ref x'
    return ()

class NewRef sr m a | sr -> a where
    -- |Construct a new reference to the provided value.
    newReference :: a -> m sr

class HasRef m where
    -- |Construct a new mutable reference (of an unspecified implementation type) containing the provided value.
    newRef :: a -> m (Ref m a)
