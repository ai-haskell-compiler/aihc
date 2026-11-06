{-# LANGUAGE MagicHash #-}

module GHC.Exts.Heap.FFIClosures_ProfilingEnabled where

import Foreign
import GHC.Exts
import GHC.Exts.Heap.Closures (TsoFlags, WhatNext, WhyBlocked)
import GHC.Exts.Heap.ProfInfo.Types
import GHC.Exts.Heap.Unsupported
import Prelude

data TSOFields = TSOFields
  { tso_what_next :: WhatNext,
    tso_why_blocked :: WhyBlocked,
    tso_flags :: [TsoFlags],
    tso_threadId :: Word64,
    tso_saved_errno :: Word32,
    tso_dirty :: Word32,
    tso_alloc_limit :: Int64,
    tso_tot_stack_size :: Word32,
    tso_prof :: Maybe StgTSOProfInfo
  }

-- | AIHC has no GHC thread state objects.
peekTSOFields :: (Ptr a -> IO (Maybe CostCentreStack)) -> Ptr tsoPtr -> IO TSOFields
peekTSOFields _ _ = unsupported "peekTSOFields"

-- | AIHC has no GHC thread state objects.
parseWhatNext :: Word16 -> WhatNext
parseWhatNext _ = unsupported "parseWhatNext"

-- | AIHC has no GHC thread state objects.
parseWhyBlocked :: Word16 -> WhyBlocked
parseWhyBlocked _ = unsupported "parseWhyBlocked"

-- | AIHC has no GHC thread state objects.
parseTsoFlags :: Word32 -> [TsoFlags]
parseTsoFlags _ = unsupported "parseTsoFlags"

data StackFields = StackFields
  { stack_size :: Word32,
    stack_dirty :: Word8,
    stack_marking :: Word8,
    stack_sp :: Addr#
  }

-- | AIHC has no GHC stack objects.
peekStackFields :: Ptr a -> IO StackFields
peekStackFields _ = unsupported "peekStackFields"
