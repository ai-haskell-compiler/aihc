{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Inspection of heap objects. AIHC heap objects do not have the layout of
-- GHC heap objects. Thus, only the primitive values can be described. Each
-- other function raises an exception.
module GHC.Exts.Heap
  ( Closure,
    GenClosure (..),
    ClosureType (..),
    PrimType (..),
    WhatNext (..),
    WhyBlocked (..),
    TsoFlags (..),
    HasHeapRep (getClosureData),
    getClosureDataFromHeapRep,
    getClosureDataFromHeapRepPrim,
    StgInfoTable (..),
    EntryFunPtr,
    HalfWord,
    ItblCodes,
    itblSize,
    peekItbl,
    pokeItbl,
    StgTSOProfInfo (..),
    IndexTable (..),
    CostCentre (..),
    CostCentreStack (..),
    getBoxedClosureData,
    allClosures,
    Box (..),
    asBox,
    areBoxesEqual,
  )
where

import Foreign (Ptr)
import GHC.Exts
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Heap.ProfInfo.Types
import GHC.Exts.Heap.Unsupported
import GHC.Int
import GHC.Word
import Prelude

-- | Types of which a value has a description as a 'Closure'.
class HasHeapRep (a :: TYPE rep) where
  -- | Describe a value.
  getClosureData :: a -> IO Closure

-- | AIHC cannot describe a boxed object.
instance HasHeapRep (a :: TYPE ('BoxedRep 'Lifted)) where
  getClosureData _ = unsupported "getClosureData"

-- | AIHC cannot describe a boxed object.
instance HasHeapRep (a :: TYPE ('BoxedRep 'Unlifted)) where
  getClosureData _ = unsupported "getClosureData"

instance (Int# ~ a) => HasHeapRep (a :: TYPE 'IntRep) where
  getClosureData x = pure IntClosure {ptipe = PInt, intVal = I# x}

instance (Word# ~ a) => HasHeapRep (a :: TYPE 'WordRep) where
  getClosureData x = pure WordClosure {ptipe = PWord, wordVal = W# x}

instance (Int64# ~ a) => HasHeapRep (a :: TYPE 'Int64Rep) where
  getClosureData x = pure Int64Closure {ptipe = PInt64, int64Val = I64# x}

instance (Word64# ~ a) => HasHeapRep (a :: TYPE 'Word64Rep) where
  getClosureData x = pure Word64Closure {ptipe = PWord64, word64Val = W64# x}

instance (Addr# ~ a) => HasHeapRep (a :: TYPE 'AddrRep) where
  getClosureData x = pure AddrClosure {ptipe = PAddr, addrVal = Ptr x}

instance (Float# ~ a) => HasHeapRep (a :: TYPE 'FloatRep) where
  getClosureData x = pure FloatClosure {ptipe = PFloat, floatVal = F# x}

instance (Double# ~ a) => HasHeapRep (a :: TYPE 'DoubleRep) where
  getClosureData x = pure DoubleClosure {ptipe = PDouble, doubleVal = D# x}

-- | Describe a closure from its raw bytes. AIHC closures do not have the GHC
-- layout.
getClosureDataFromHeapRep :: ByteArray# -> Ptr StgInfoTable -> [b] -> IO (GenClosure b)
getClosureDataFromHeapRep _ _ _ = unsupported "getClosureDataFromHeapRep"

-- | Describe a closure from its raw bytes, with functions that read the name
-- of a constructor and a cost-centre stack. AIHC closures do not have the GHC
-- layout.
getClosureDataFromHeapRepPrim ::
  IO (String, String, String) ->
  (Ptr b -> IO (Maybe CostCentreStack)) ->
  StgInfoTable ->
  ByteArray# ->
  [b] ->
  IO (GenClosure b)
getClosureDataFromHeapRepPrim _ _ _ _ _ = unsupported "getClosureDataFromHeapRepPrim"

-- | Describe the value in a 'Box'.
getBoxedClosureData :: Box -> IO Closure
getBoxedClosureData (Box a) = getClosureData a
