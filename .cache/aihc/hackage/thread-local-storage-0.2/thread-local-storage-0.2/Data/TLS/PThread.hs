{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | An implementation of TLS using the standard, Posix
-- `pthread_create_key` routine.
--
-- Note that because this implementation uses Posix threads, it does
-- NOT care about Haskell IO threads.  This module is generally used
-- to avoid problems with data structures or other APIs that are not
-- thread safe.  That is, pthread-based TLS is sufficient to disallow
-- simultaneous access, irrespective of where IO threads migrate to.

module Data.TLS.PThread
    ( TLS
    , mkTLS
    , getTLS
    , allTLS
    , forEachTLS_
    , freeAllTLS
    -- * Deprecated, backwards compatibility
    , freeTLS
    )
    where

-- Same as the internal module but with limited export:
import Data.TLS.PThread.Internal
