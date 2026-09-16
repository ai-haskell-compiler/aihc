{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foreign.C.Types
  ( CBool (..),
    CChar (..),
    CClock (..),
    CDouble (..),
    CFile,
    CFloat (..),
    CFpos,
    CInt (..),
    CIntMax (..),
    CIntPtr (..),
    CJmpBuf,
    CLLong (..),
    CLong (..),
    CPtrdiff (..),
    CSChar (..),
    CSUSeconds (..),
    CShort (..),
    CSigAtomic (..),
    CSize (..),
    CTime (..),
    CUChar (..),
    CUInt (..),
    CUIntMax (..),
    CUIntPtr (..),
    CULLong (..),
    CULong (..),
    CUSeconds (..),
    CUShort (..),
    CWchar (..),
  )
where

import Data.Bits (Bits, FiniteBits)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types.Repr
  ( CIntPtrRep,
    CLongRep,
    CPtrdiffRep,
    CSizeRep,
    CUIntPtrRep,
    CULongRep,
  )
import Foreign.Storable (Storable)
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Float ()
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Read (Read)
import GHC.Real (Integral (..), Real (..))
import GHC.Show (Show)
import GHC.Types (Double, Float)

newtype CBool = CBool Word8
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CChar = CChar Int8
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CClock = CClock Word64
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CDouble = CDouble Double
  deriving newtype (Eq, Ord, Show, Read, Num, Storable)

data CFile = CFile

newtype CFloat = CFloat Float
  deriving newtype (Eq, Ord, Show, Read, Num, Storable)

data CFpos = CFpos

newtype CInt = CInt Int32
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CIntMax = CIntMax Int64
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CIntPtr = CIntPtr CIntPtrRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

data CJmpBuf = CJmpBuf

newtype CLLong = CLLong Int64
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CLong = CLong CLongRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CPtrdiff = CPtrdiff CPtrdiffRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CSChar = CSChar Int8
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CSUSeconds = CSUSeconds Int32
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CShort = CShort Int16
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CSigAtomic = CSigAtomic Int32
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CSize = CSize CSizeRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CTime = CTime Int64
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CUChar = CUChar Word8
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CUInt = CUInt Word32
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CUIntMax = CUIntMax Word64
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CUIntPtr = CUIntPtr CUIntPtrRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CULLong = CULLong Word64
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CULong = CULong CULongRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CUSeconds = CUSeconds Word32
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CUShort = CUShort Word16
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

newtype CWchar = CWchar Int32
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)
