module GHC.Exts.Heap.Constants
  ( wORD_SIZE,
    tAG_MASK,
    wORD_SIZE_IN_BITS,
  )
where

import Prelude

-- | The size of a machine word in bytes. AIHC targets 64-bit machines.
wORD_SIZE :: Int
wORD_SIZE = 8

-- | The size of a machine word in bits.
wORD_SIZE_IN_BITS :: Int
wORD_SIZE_IN_BITS = 64

-- | The mask for the tag in the three low bits of a pointer.
tAG_MASK :: Int
tAG_MASK = 7
