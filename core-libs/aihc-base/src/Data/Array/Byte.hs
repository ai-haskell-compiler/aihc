{-# LANGUAGE MagicHash #-}

module Data.Array.Byte
  ( ByteArray (..),
    MutableByteArray (..),
  )
where

import GHC.Prim (ByteArray#, MutableByteArray#)

-- | Lifted wrapper for an immutable primitive byte array.
--
-- The primitive 'ByteArray#' is unlifted, so it cannot be stored directly in
-- ordinary lifted data structures.  This wrapper is the public representation
-- used by @base@ and packages such as @deepseq@.
data ByteArray = ByteArray ByteArray#

-- | Lifted wrapper for a mutable primitive byte array in state thread @s@.
--
-- Keeping @s@ on both layers prevents a mutable array from escaping the state
-- thread that owns it.
data MutableByteArray s = MutableByteArray (MutableByteArray# s)
