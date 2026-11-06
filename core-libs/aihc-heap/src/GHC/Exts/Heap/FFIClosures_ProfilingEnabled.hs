{-# LANGUAGE MagicHash #-}

-- | Readers for the thread and stack records of the GHC runtime system.
-- AIHC has no such records, so each reader raises an exception.
module GHC.Exts.Heap.FFIClosures_ProfilingEnabled where

import Data.Bits (complement, (.&.))
import Foreign
import GHC.Exts (Addr#)
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

peekTSOFields :: (Ptr a -> IO (Maybe CostCentreStack)) -> Ptr tsoPtr -> IO TSOFields
peekTSOFields _ _ = unsupported "peekTSOFields"

parseWhatNext :: Word16 -> WhatNext
parseWhatNext _ = unsupported "parseWhatNext"

parseWhyBlocked :: Word16 -> WhyBlocked
parseWhyBlocked _ = unsupported "parseWhyBlocked"

parseTsoFlags :: Word32 -> [TsoFlags]
parseTsoFlags _ = unsupported "parseTsoFlags"

data StackFields = StackFields
  { stack_size :: Word32,
    stack_dirty :: Word8,
    stack_marking :: Word8,
    stack_sp :: Addr#
  }

peekStackFields :: Ptr a -> IO StackFields
peekStackFields _ = unsupported "peekStackFields"

-- | Test if all bits of the mask are set in the word.
isSet :: Word32 -> Word32 -> Bool
isSet mask w = w .&. mask == mask

-- | Clear the bits of the mask in the word.
unset :: Word32 -> Word32 -> Word32
unset mask w = w .&. complement mask
