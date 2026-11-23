-- | A call to a C function whose type is known only at run time.
--
-- The system libffi makes the call. The C file @cbits/foreign_call.c@ owns
-- all libffi structures. Thus this module needs no hsc2hs step and no
-- libffi header.
module Aihc.Testing.ForeignCall
  ( Arg,
    RetType,
    callFFI,
    argInt8,
    argInt16,
    argInt32,
    argInt64,
    argWord8,
    argWord16,
    argWord32,
    argWord64,
    argCFloat,
    argCDouble,
    argPtr,
    retVoid,
    retInt8,
    retInt16,
    retInt32,
    retInt64,
    retWord8,
    retWord16,
    retWord32,
    retWord64,
    retCFloat,
    retCDouble,
    retPtr,
  )
where

import Control.Monad (when)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CDouble (..), CFloat (..), CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, WordPtr (..), castFunPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (peek)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)

-- | One argument of a call: the kind of its C type and its bits.
data Arg = Arg !CInt !Word64

-- | The C type of a result, and the function that reads the result bits.
data RetType a = RetType !CInt (Word64 -> a)

instance Functor RetType where
  fmap f (RetType kind decode) = RetType kind (f . decode)

-- The kind numbers are the same as in @cbits/foreign_call.c@.

argInt8 :: Int8 -> Arg
argInt8 = Arg 1 . fromIntegral

argWord8 :: Word8 -> Arg
argWord8 = Arg 2 . fromIntegral

argInt16 :: Int16 -> Arg
argInt16 = Arg 3 . fromIntegral

argWord16 :: Word16 -> Arg
argWord16 = Arg 4 . fromIntegral

argInt32 :: Int32 -> Arg
argInt32 = Arg 5 . fromIntegral

argWord32 :: Word32 -> Arg
argWord32 = Arg 6 . fromIntegral

argInt64 :: Int64 -> Arg
argInt64 = Arg 7 . fromIntegral

argWord64 :: Word64 -> Arg
argWord64 = Arg 8

argCFloat :: CFloat -> Arg
argCFloat (CFloat value) = Arg 9 (fromIntegral (castFloatToWord32 value))

argCDouble :: CDouble -> Arg
argCDouble (CDouble value) = Arg 10 (castDoubleToWord64 value)

argPtr :: Ptr a -> Arg
argPtr pointer = Arg 11 (fromIntegral (unWordPtr (ptrToWordPtr pointer)))
  where
    unWordPtr (WordPtr word) = word

retVoid :: RetType ()
retVoid = RetType 0 (const ())

retInt8 :: RetType Int8
retInt8 = RetType 1 fromIntegral

retWord8 :: RetType Word8
retWord8 = RetType 2 fromIntegral

retInt16 :: RetType Int16
retInt16 = RetType 3 fromIntegral

retWord16 :: RetType Word16
retWord16 = RetType 4 fromIntegral

retInt32 :: RetType Int32
retInt32 = RetType 5 fromIntegral

retWord32 :: RetType Word32
retWord32 = RetType 6 fromIntegral

retInt64 :: RetType Int64
retInt64 = RetType 7 fromIntegral

retWord64 :: RetType Word64
retWord64 = RetType 8 id

retCFloat :: RetType CFloat
retCFloat = RetType 9 (CFloat . castWord32ToFloat . fromIntegral)

retCDouble :: RetType CDouble
retCDouble = RetType 10 (CDouble . castWord64ToDouble)

retPtr :: RetType (Ptr a)
retPtr = RetType 11 (wordPtrToPtr . WordPtr . fromIntegral)

-- | Call a C function with the given result type and arguments.
callFFI :: FunPtr a -> RetType b -> [Arg] -> IO b
callFFI function (RetType resultKind decode) arguments =
  withArrayLen [kind | Arg kind _ <- arguments] $ \count kinds ->
    withArrayLen [bits | Arg _ bits <- arguments] $ \_ values ->
      alloca $ \result -> do
        status <- c_foreign_call (castFunPtr function) resultKind result (fromIntegral count) kinds values
        when (status /= 0) $
          ioError (userError "callFFI: libffi did not accept the call")
        decode <$> peek result

foreign import ccall safe "aihc_testing_foreign_call"
  c_foreign_call :: FunPtr (IO ()) -> CInt -> Ptr Word64 -> CUInt -> Ptr CInt -> Ptr Word64 -> IO CInt
