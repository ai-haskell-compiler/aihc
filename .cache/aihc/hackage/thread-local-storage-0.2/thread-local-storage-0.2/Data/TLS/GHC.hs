{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

-- | This is the simplest implementation of thread-local storage using
-- GHC's built-in ThreadId capabilities.
--
-- While the interface below is designed to match the other
-- implementations of TLS in this package, the GHC implementation can
-- generaly be relied upon to keep a stable copy of the TLS variable
-- for each `ThreadId` that calls `getTLS`.  This may change in the
-- future, however!

module Data.TLS.GHC
    ( TLS
    , mkTLS
    , getTLS
    , allTLS
    , forEachTLS_
    , freeAllTLS
    -- * Deprecated, backwards compatibility
    , freeTLS
    ) where

import Control.Monad
import Control.Concurrent
import Data.Map.Strict as M
import Data.IORef

-- Module signature:
--------------------------------------------------------------------------------

#include "TLS_Sig.hs"

--------------------------------------------------------------------------------

-- | A thread-local variable of type `a`.
data TLS a = TLS { mkNew     ::  !(IO a)
                 , allCopies :: {-# UNPACK #-} !(IORef (Map ThreadId a)) }

mkTLS new = do
  v <- newIORef $! M.empty
  return $! TLS new v

getTLS TLS{mkNew,allCopies} = do
    tid  <- myThreadId
    peek <- readIORef allCopies
    case M.lookup tid peek of
      Just a  -> return a
      Nothing -> do
        a <- mkNew
        atomicModifyIORef' allCopies (\ mp -> (M.insert tid a mp, ()))
        return $! a

allTLS TLS{allCopies} = do
  mp <- readIORef allCopies
  return $! M.elems mp

forEachTLS_ tls fn = do
  ls <- allTLS tls
  forM_ ls fn 

-- Nothing to do here... we haven't pinned anything.  Normal GC is fine.
freeAllTLS _ =
  do return ()

