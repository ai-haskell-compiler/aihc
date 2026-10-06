-- | Element reads and writes at a pointer, one pair for each primitive type.
module GHC.Storable
  ( readWideCharOffPtr,
    writeWideCharOffPtr,
    readIntOffPtr,
    writeIntOffPtr,
    readWordOffPtr,
    writeWordOffPtr,
    readPtrOffPtr,
    writePtrOffPtr,
    readFunPtrOffPtr,
    writeFunPtrOffPtr,
    readFloatOffPtr,
    writeFloatOffPtr,
    readDoubleOffPtr,
    writeDoubleOffPtr,
    readStablePtrOffPtr,
    writeStablePtrOffPtr,
    readInt8OffPtr,
    writeInt8OffPtr,
    readInt16OffPtr,
    writeInt16OffPtr,
    readInt32OffPtr,
    writeInt32OffPtr,
    readInt64OffPtr,
    writeInt64OffPtr,
    readWord8OffPtr,
    writeWord8OffPtr,
    readWord16OffPtr,
    writeWord16OffPtr,
    readWord32OffPtr,
    writeWord32OffPtr,
    readWord64OffPtr,
    writeWord64OffPtr,
  )
where

import Foreign.Storable (Storable (..))
import GHC.IO (IO)
import GHC.Int (Int, Int16, Int32, Int64, Int8)
import GHC.Ptr (FunPtr, Ptr)
import GHC.Stable (StablePtr)
import GHC.Types (Char, Double, Float)
import GHC.Word (Word, Word16, Word32, Word64, Word8)

-- | Read an element of a pointer to @Char@.
readWideCharOffPtr :: Ptr Char -> Int -> IO Char
readWideCharOffPtr = peekElemOff

-- | Write an element of a pointer to @Char@.
writeWideCharOffPtr :: Ptr Char -> Int -> Char -> IO ()
writeWideCharOffPtr = pokeElemOff

-- | Read an element of a pointer to @Int@.
readIntOffPtr :: Ptr Int -> Int -> IO Int
readIntOffPtr = peekElemOff

-- | Write an element of a pointer to @Int@.
writeIntOffPtr :: Ptr Int -> Int -> Int -> IO ()
writeIntOffPtr = pokeElemOff

-- | Read an element of a pointer to @Word@.
readWordOffPtr :: Ptr Word -> Int -> IO Word
readWordOffPtr = peekElemOff

-- | Write an element of a pointer to @Word@.
writeWordOffPtr :: Ptr Word -> Int -> Word -> IO ()
writeWordOffPtr = pokeElemOff

-- | Read an element of a pointer to @(Ptr a)@.
readPtrOffPtr :: Ptr (Ptr a) -> Int -> IO (Ptr a)
readPtrOffPtr = peekElemOff

-- | Write an element of a pointer to @(Ptr a)@.
writePtrOffPtr :: Ptr (Ptr a) -> Int -> Ptr a -> IO ()
writePtrOffPtr = pokeElemOff

-- | Read an element of a pointer to @(FunPtr a)@.
readFunPtrOffPtr :: Ptr (FunPtr a) -> Int -> IO (FunPtr a)
readFunPtrOffPtr = peekElemOff

-- | Write an element of a pointer to @(FunPtr a)@.
writeFunPtrOffPtr :: Ptr (FunPtr a) -> Int -> FunPtr a -> IO ()
writeFunPtrOffPtr = pokeElemOff

-- | Read an element of a pointer to @Float@.
readFloatOffPtr :: Ptr Float -> Int -> IO Float
readFloatOffPtr = peekElemOff

-- | Write an element of a pointer to @Float@.
writeFloatOffPtr :: Ptr Float -> Int -> Float -> IO ()
writeFloatOffPtr = pokeElemOff

-- | Read an element of a pointer to @Double@.
readDoubleOffPtr :: Ptr Double -> Int -> IO Double
readDoubleOffPtr = peekElemOff

-- | Write an element of a pointer to @Double@.
writeDoubleOffPtr :: Ptr Double -> Int -> Double -> IO ()
writeDoubleOffPtr = pokeElemOff

-- | Read an element of a pointer to @(StablePtr a)@.
readStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> IO (StablePtr a)
readStablePtrOffPtr = peekElemOff

-- | Write an element of a pointer to @(StablePtr a)@.
writeStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> StablePtr a -> IO ()
writeStablePtrOffPtr = pokeElemOff

-- | Read an element of a pointer to @Int8@.
readInt8OffPtr :: Ptr Int8 -> Int -> IO Int8
readInt8OffPtr = peekElemOff

-- | Write an element of a pointer to @Int8@.
writeInt8OffPtr :: Ptr Int8 -> Int -> Int8 -> IO ()
writeInt8OffPtr = pokeElemOff

-- | Read an element of a pointer to @Int16@.
readInt16OffPtr :: Ptr Int16 -> Int -> IO Int16
readInt16OffPtr = peekElemOff

-- | Write an element of a pointer to @Int16@.
writeInt16OffPtr :: Ptr Int16 -> Int -> Int16 -> IO ()
writeInt16OffPtr = pokeElemOff

-- | Read an element of a pointer to @Int32@.
readInt32OffPtr :: Ptr Int32 -> Int -> IO Int32
readInt32OffPtr = peekElemOff

-- | Write an element of a pointer to @Int32@.
writeInt32OffPtr :: Ptr Int32 -> Int -> Int32 -> IO ()
writeInt32OffPtr = pokeElemOff

-- | Read an element of a pointer to @Int64@.
readInt64OffPtr :: Ptr Int64 -> Int -> IO Int64
readInt64OffPtr = peekElemOff

-- | Write an element of a pointer to @Int64@.
writeInt64OffPtr :: Ptr Int64 -> Int -> Int64 -> IO ()
writeInt64OffPtr = pokeElemOff

-- | Read an element of a pointer to @Word8@.
readWord8OffPtr :: Ptr Word8 -> Int -> IO Word8
readWord8OffPtr = peekElemOff

-- | Write an element of a pointer to @Word8@.
writeWord8OffPtr :: Ptr Word8 -> Int -> Word8 -> IO ()
writeWord8OffPtr = pokeElemOff

-- | Read an element of a pointer to @Word16@.
readWord16OffPtr :: Ptr Word16 -> Int -> IO Word16
readWord16OffPtr = peekElemOff

-- | Write an element of a pointer to @Word16@.
writeWord16OffPtr :: Ptr Word16 -> Int -> Word16 -> IO ()
writeWord16OffPtr = pokeElemOff

-- | Read an element of a pointer to @Word32@.
readWord32OffPtr :: Ptr Word32 -> Int -> IO Word32
readWord32OffPtr = peekElemOff

-- | Write an element of a pointer to @Word32@.
writeWord32OffPtr :: Ptr Word32 -> Int -> Word32 -> IO ()
writeWord32OffPtr = pokeElemOff

-- | Read an element of a pointer to @Word64@.
readWord64OffPtr :: Ptr Word64 -> Int -> IO Word64
readWord64OffPtr = peekElemOff

-- | Write an element of a pointer to @Word64@.
writeWord64OffPtr :: Ptr Word64 -> Int -> Word64 -> IO ()
writeWord64OffPtr = pokeElemOff
