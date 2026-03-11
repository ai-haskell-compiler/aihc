{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language DerivingStrategies #-}
{-# language StandaloneDeriving #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language CPP #-}

module Data.Primitive.ByteArray.Unaligned
  ( -- * Class
    PrimUnaligned(..)
    -- * Array access
  , indexUnalignedByteArray
  , readUnalignedByteArray
  , writeUnalignedByteArray
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Monad.Primitive (primitive,primitive_)
import Data.Primitive.ByteArray (MutableByteArray(..))
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Word (Word8,Word64)
import Data.Int (Int8,Int64)
import GHC.Int (Int16(I16#),Int32(I32#),Int(I#))
import GHC.Word (Word16(W16#),Word32(W32#),Word(W#))
import GHC.Exts (Int#,State#,MutableByteArray#,ByteArray#)
import GHC.Exts (Char(C#),Double(D#),Float(F#),Ptr(Ptr))
import qualified Foreign.C.Types as C
import qualified System.Posix.Types as P
import qualified Data.Primitive.Unaligned.Mach as M
import qualified Data.Primitive as PM
import qualified GHC.Exts as E

-- | Class of types supporting primitive array operations
-- that are not necessarily aligned. The offsets for all
-- of the typeclass methods are interpreted as bytes,
-- not elements.
class PrimUnaligned a where
  indexUnalignedByteArray# ::
    ByteArray# -> Int# -> a
  readUnalignedByteArray# ::
       MutableByteArray# s
    -> Int#
    -> State# s
    -> (# State# s, a #)
  writeUnalignedByteArray# ::
       MutableByteArray# s
    -> Int#
    -> a
    -> State# s
    -> State# s

instance PrimUnaligned Word8 where
  indexUnalignedByteArray# = PM.indexByteArray#
  readUnalignedByteArray# = PM.readByteArray#
  writeUnalignedByteArray# = PM.writeByteArray#

instance PrimUnaligned Word16 where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    W16# (E.indexWord8ArrayAsWord16# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsWord16# a i s0 of
      (# s1, r #) -> (# s1, W16# r #)
  writeUnalignedByteArray# a i (W16# w) =
    E.writeWord8ArrayAsWord16# a i w

instance PrimUnaligned Word32 where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    W32# (E.indexWord8ArrayAsWord32# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsWord32# a i s0 of
      (# s1, r #) -> (# s1, W32# r #)
  writeUnalignedByteArray# a i (W32# w) =
    E.writeWord8ArrayAsWord32# a i w

instance PrimUnaligned Word where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    W# (E.indexWord8ArrayAsWord# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsWord# a i s0 of
      (# s1, r #) -> (# s1, W# r #)
  writeUnalignedByteArray# a i (W# w) =
    E.writeWord8ArrayAsWord# a i w

instance PrimUnaligned Word64 where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# = M.indexUnalignedWord64Array#
  readUnalignedByteArray# = M.readUnalignedWord64Array#
  writeUnalignedByteArray# = M.writeUnalignedWord64Array#

instance PrimUnaligned Int8 where
  indexUnalignedByteArray# = PM.indexByteArray#
  readUnalignedByteArray# = PM.readByteArray#
  writeUnalignedByteArray# = PM.writeByteArray#

instance PrimUnaligned Int16 where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    I16# (E.indexWord8ArrayAsInt16# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsInt16# a i s0 of
      (# s1, r #) -> (# s1, I16# r #)
  writeUnalignedByteArray# a i (I16# w) =
    E.writeWord8ArrayAsInt16# a i w

instance PrimUnaligned Int32 where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    I32# (E.indexWord8ArrayAsInt32# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsInt32# a i s0 of
      (# s1, r #) -> (# s1, I32# r #)
  writeUnalignedByteArray# a i (I32# w) =
    E.writeWord8ArrayAsInt32# a i w

instance PrimUnaligned Int where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    I# (E.indexWord8ArrayAsInt# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsInt# a i s0 of
      (# s1, r #) -> (# s1, I# r #)
  writeUnalignedByteArray# a i (I# w) =
    E.writeWord8ArrayAsInt# a i w

instance PrimUnaligned Int64 where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# = M.indexUnalignedInt64Array#
  readUnalignedByteArray# = M.readUnalignedInt64Array#
  writeUnalignedByteArray# = M.writeUnalignedInt64Array#

instance PrimUnaligned Char where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    C# (E.indexWord8ArrayAsWideChar# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsWideChar# a i s0 of
      (# s1, r #) -> (# s1, C# r #)
  writeUnalignedByteArray# a i (C# w) =
    E.writeWord8ArrayAsWideChar# a i w

instance PrimUnaligned Double where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    D# (E.indexWord8ArrayAsDouble# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsDouble# a i s0 of
      (# s1, r #) -> (# s1, D# r #)
  writeUnalignedByteArray# a i (D# w) =
    E.writeWord8ArrayAsDouble# a i w

instance PrimUnaligned Float where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    F# (E.indexWord8ArrayAsFloat# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsFloat# a i s0 of
      (# s1, r #) -> (# s1, F# r #)
  writeUnalignedByteArray# a i (F# w) =
    E.writeWord8ArrayAsFloat# a i w

instance PrimUnaligned (Ptr a) where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i =
    Ptr (E.indexWord8ArrayAsAddr# a i)
  readUnalignedByteArray# a i s0 =
    case E.readWord8ArrayAsAddr# a i s0 of
      (# s1, r #) -> (# s1, Ptr r #)
  writeUnalignedByteArray# a i (Ptr w) =
    E.writeWord8ArrayAsAddr# a i w

#if defined(HTYPE_CC_T)
deriving newtype instance PrimUnaligned P.CCc
#endif
#if defined(HTYPE_GID_T)
deriving newtype instance PrimUnaligned P.CGid
#endif
#if defined(HTYPE_NLINK_T)
deriving newtype instance PrimUnaligned P.CNlink
#endif
#if defined(HTYPE_UID_T)
deriving newtype instance PrimUnaligned P.CUid
#endif

deriving newtype instance PrimUnaligned C.CChar
deriving newtype instance PrimUnaligned P.CDev
deriving newtype instance PrimUnaligned C.CDouble
deriving newtype instance PrimUnaligned P.CIno
deriving newtype instance PrimUnaligned C.CInt
deriving newtype instance PrimUnaligned C.CLLong
deriving newtype instance PrimUnaligned C.CLong
deriving newtype instance PrimUnaligned P.CMode
deriving newtype instance PrimUnaligned P.COff
deriving newtype instance PrimUnaligned P.CPid
deriving newtype instance PrimUnaligned C.CSChar
deriving newtype instance PrimUnaligned P.CSsize
deriving newtype instance PrimUnaligned C.CShort
deriving newtype instance PrimUnaligned C.CUInt
deriving newtype instance PrimUnaligned C.CULLong
deriving newtype instance PrimUnaligned C.CULong
deriving newtype instance PrimUnaligned P.Fd

-- | Read a primitive value from the byte array.
-- The offset is given in bytes rather than in elements
-- of type @a@.
indexUnalignedByteArray ::
     PrimUnaligned a
  => ByteArray -- ^ Immutable array
  -> Int -- ^ Offset in bytes
  -> a
indexUnalignedByteArray (ByteArray a) (I# i) =
  indexUnalignedByteArray# a i

-- | Read a primitive value from the byte array.
-- The offset is given in bytes rather than in elements
-- of type @a@.
readUnalignedByteArray ::
     (PrimMonad m, PrimUnaligned a)
  => MutableByteArray (PrimState m) -- ^ Mutable array
  -> Int -- ^ Offset in bytes
  -> m a
readUnalignedByteArray (MutableByteArray a) (I# i) =
  primitive (readUnalignedByteArray# a i)

-- | Write a primitive value to the byte array.
-- The offset is given in bytes rather than in elements
-- of type @a@.
writeUnalignedByteArray ::
     (PrimMonad m, PrimUnaligned a)
  => MutableByteArray (PrimState m) -- ^ Mutable array
  -> Int -- ^ Offset in bytes
  -> a -- ^ Element
  -> m ()
writeUnalignedByteArray (MutableByteArray a) (I# i) x =
  primitive_ (writeUnalignedByteArray# a i x)
