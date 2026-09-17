{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Synchronised variables and the bracketed operations over them.
--
-- Each combinator puts the value back when the action it runs raises, so a
-- failing action leaves the 'MVar' full rather than deadlocking whoever
-- takes it next.
module Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    isEmptyMVar,
    tryPutMVar,
    tryReadMVar,
    tryTakeMVar,
    swapMVar,
    withMVar,
    withMVarMasked,
    modifyMVar,
    modifyMVar_,
    modifyMVarMasked,
    modifyMVarMasked_,
    mkWeakMVar,
    addMVarFinalizer,
  )
where

import GHC.Base (Monad (..))
import GHC.IO (IO (..), evaluate, mask, mask_, onException, unIO)
import GHC.MVar
  ( MVar (..),
    addMVarFinalizer,
    isEmptyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryReadMVar,
    tryTakeMVar,
  )
import GHC.Prim (mkWeak#)
import GHC.Weak (Weak (..))

-- | Replace the contents and return what was there.
swapMVar :: MVar a -> a -> IO a
swapMVar mvar replacement =
  mask_
    ( do
        current <- takeMVar mvar
        putMVar mvar replacement
        return current
    )

-- | Run an action on the contents, putting them back afterwards.
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar mvar action =
  mask
    ( \restore -> do
        current <- takeMVar mvar
        result <- onException (restore (action current)) (putMVar mvar current)
        putMVar mvar current
        return result
    )

-- | 'withMVar' that leaves the action masked.
withMVarMasked :: MVar a -> (a -> IO b) -> IO b
withMVarMasked mvar action =
  mask_
    ( do
        current <- takeMVar mvar
        result <- onException (action current) (putMVar mvar current)
        putMVar mvar current
        return result
    )

-- | Replace the contents with what the action gives.
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ mvar action =
  mask
    ( \restore -> do
        current <- takeMVar mvar
        replacement <- onException (restore (action current)) (putMVar mvar current)
        putMVar mvar replacement
    )

-- | 'modifyMVar_' whose action also gives a result.
--
-- The pair is forced before the 'MVar' is filled, so an action that fails
-- while producing it leaves the old contents in place.
modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar mvar action =
  mask
    ( \restore -> do
        current <- takeMVar mvar
        pair <- onException (restore (action current >>= evaluate)) (putMVar mvar current)
        case pair of
          (replacement, result) -> do
            putMVar mvar replacement
            return result
    )

-- | 'modifyMVar_' that leaves the action masked.
modifyMVarMasked_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ mvar action =
  mask_
    ( do
        current <- takeMVar mvar
        replacement <- onException (action current) (putMVar mvar current)
        putMVar mvar replacement
    )

-- | 'modifyMVar' that leaves the action masked.
modifyMVarMasked :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVarMasked mvar action =
  mask_
    ( do
        current <- takeMVar mvar
        pair <- onException (action current >>= evaluate) (putMVar mvar current)
        case pair of
          (replacement, result) -> do
            putMVar mvar replacement
            return result
    )

-- | A weak pointer to the 'MVar' whose finalizer runs when the 'MVar'
-- becomes unreachable, and which 'System.Mem.Weak.finalize' runs it through
-- before then.
--
-- The pointer is keyed on the 'MVar#' itself, as GHC's is. GHC would then
-- let the 'MVar' go once nothing else held it; this runtime collects no
-- weak pointer, so a pointer made here keeps its 'MVar' alive until the
-- program ends.
mkWeakMVar :: MVar a -> IO () -> IO (Weak (MVar a))
mkWeakMVar mvar@(MVar rawMVar) finalizer =
  IO
    ( \state ->
        case mkWeak# rawMVar mvar (unIO finalizer) state of
          (# nextState, weak #) -> (# nextState, Weak weak #)
    )
