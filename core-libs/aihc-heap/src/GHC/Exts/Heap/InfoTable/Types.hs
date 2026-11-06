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
import GHC.Generics (Generic)
import Prelude

-- | The bytes of code that follow an info table.
type ItblCodes = Either [Word8] [Word32]

-- | Half of a machine word. AIHC targets 64-bit machines.
newtype HalfWord = HalfWord Word32
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show, Storable)

-- | A pointer to the entry code of a closure.
type EntryFunPtr = FunPtr (Ptr () -> IO (Ptr ()))

-- | The fields of an info table in the GHC runtime system. AIHC info tables
-- have another layout, so a program cannot read a value of this type from a
-- running AIHC program.
data StgInfoTable = StgInfoTable
  { entry :: Maybe EntryFunPtr,
    ptrs :: HalfWord,
    nptrs :: HalfWord,
    tipe :: ClosureType,
    srtlen :: HalfWord,
    code :: Maybe ItblCodes
  }
  deriving (Eq, Show, Generic)
