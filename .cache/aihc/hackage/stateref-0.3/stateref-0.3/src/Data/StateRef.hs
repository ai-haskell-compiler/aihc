-- |This module provides classes and instances for mutable state
-- references.  Various implementation exist in common usage, but
-- no way (until now ;-) to define functions using state references
-- which don't depend on the specific monad or reference type in use.
-- 
-- These modules use several language extensions, including multi-parameter
-- type classes and functional dependencies.
module Data.StateRef
    ( module Data.StateRef
    , module Data.StateRef.Types
    , module Data.StateRef.Instances
    , module Data.Accessor
    ) where

import Data.StateRef.Types
import Data.StateRef.Instances
import Data.Accessor

-- |Read a 'Ref'.  See 'readReference'.
readRef :: Ref m a -> m a
readRef = readReference

-- |Write a 'Ref'.  See 'writeReference'
writeRef :: Ref m a -> a -> m ()
writeRef = writeReference

-- |Modify a 'Ref'.  See 'modifyReference'.
atomicModifyRef :: Ref m a -> (a -> (a,b)) -> m b
atomicModifyRef = atomicModifyReference


-- |Modify a 'Ref'.  See 'modifyReference'.
modifyRef :: Ref m a -> (a -> a) -> m ()
modifyRef = modifyReference


-- |Essentially the same concept as 'Control.Monad.State.gets',
-- 'Control.Monad.State.asks', et al. Typically useful to read a field of
-- a referenced ADT by passing a record selector as the second argument.
readsRef :: (ReadRef sr m a,
             Monad m) =>
            sr -> (a -> b) -> m b
readsRef r f = do
    x <- readReference r
    return (f x)

-- |Construct a counter - a monadic value which, each time it is
-- evaluated, returns the 'succ' of the previous value returned.
newCounter :: (HasRef m, Monad m, Enum a) => a -> m (m a)
newCounter n = do
    c <- newRef n
    return $ do
        x <- readRef c
        writeRef c (succ x)
        return x

-- |Create a \"lapse reader\" (suggestions for better terminology are more 
-- than welcome), a sort of a time-lapse of the variable.  The first 
-- motivating instance for this operation was a clock in a simple simulation
-- application.  Given a 'TVar' 'Double' called \"clock\", a useful
-- value \"dT\" is yielded by the expression: 'mkLapseReader' clock (-)
mkLapseReader
  :: (ReadRef sr m a, HasRef m, Monad m) =>
     sr -> (a -> a -> b) -> m (m b)
mkLapseReader var f = do
    startVal <- readReference var
    prevRef <- newRef startVal
    
    return $ do
        newVal <- readReference var
        prevVal <- readRef prevRef
        
        writeReference prevRef newVal
        
        return (f newVal prevVal)
