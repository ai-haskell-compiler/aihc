module GHC.Exts.Heap.Constants
  ( wORD_SIZE,
    tAG_MASK,
    wORD_SIZE_IN_BITS,
  )
where

import Data.Bits
import Prelude

-- | AIHC targets 64-bit machines only. A word has 8 bytes. The three low
-- bits of a pointer are its tag.
wORD_SIZE, tAG_MASK, wORD_SIZE_IN_BITS :: Int
wORD_SIZE = 8
wORD_SIZE_IN_BITS = 64
tAG_MASK = (1 `shift` 3) - 1
