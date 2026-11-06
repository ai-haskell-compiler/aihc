{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Exts.Heap.InfoTable.Types
  ( StgInfoTable (..),
    EntryFunPtr,
    HalfWord (..),
    ItblCodes,
  )
where

import Foreign
import GHC.Exts.Heap.ClosureTypes
import GHC.Generics
import Prelude

type ItblCodes = Either [Word8] [Word32]

-- | A half word of a 64-bit machine.
type HalfWord' = Word32

newtype HalfWord = HalfWord HalfWord'
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show, Storable)

type EntryFunPtr = FunPtr (Ptr () -> IO (Ptr ()))

-- | A representation of an info table, with the fields of GHC's
-- @StgInfoTable@. AIHC info tables have another layout.
data StgInfoTable = StgInfoTable
  { entry :: Maybe EntryFunPtr, -- Just <=> not TABLES_NEXT_TO_CODE
    ptrs :: HalfWord,
    nptrs :: HalfWord,
    tipe :: ClosureType,
    srtlen :: HalfWord,
    code :: Maybe ItblCodes -- Just <=> TABLES_NEXT_TO_CODE
  }
  deriving (Eq, Show, Generic)
