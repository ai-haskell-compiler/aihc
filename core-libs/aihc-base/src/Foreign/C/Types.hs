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

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Types (Double, Float)

newtype CBool = CBool Word8

newtype CChar = CChar Int8

newtype CClock = CClock Word64

newtype CDouble = CDouble Double

data CFile = CFile

newtype CFloat = CFloat Float

data CFpos = CFpos

newtype CInt = CInt Int32

newtype CIntMax = CIntMax Int64

newtype CIntPtr = CIntPtr Int64

data CJmpBuf = CJmpBuf

newtype CLLong = CLLong Int64

newtype CLong = CLong Int64

newtype CPtrdiff = CPtrdiff Int64

newtype CSChar = CSChar Int8

newtype CSUSeconds = CSUSeconds Int32

newtype CShort = CShort Int16

newtype CSigAtomic = CSigAtomic Int32

newtype CSize = CSize Word64

newtype CTime = CTime Int64

newtype CUChar = CUChar Word8

newtype CUInt = CUInt Word32

newtype CUIntMax = CUIntMax Word64

newtype CUIntPtr = CUIntPtr Word64

newtype CULLong = CULLong Word64

newtype CULong = CULong Word64

newtype CUSeconds = CUSeconds Word32

newtype CUShort = CUShort Word16

newtype CWchar = CWchar Int32
