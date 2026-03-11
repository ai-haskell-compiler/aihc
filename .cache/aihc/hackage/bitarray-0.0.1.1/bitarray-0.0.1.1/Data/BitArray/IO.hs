
-- | Mutable one-dimensional packed bit arrays in the IO monad.

module Data.BitArray.IO
  ( IOBitArray 
  , getBitArrayBounds
  , newBitArray
  , readBit
  , writeBit
  , flipBit
  , unsafeReadBit
  , unsafeWriteBit
  , unsafeFlipBit
  
  , thawBitArray
  , unsafeThawBitArray
  , freezeBitArray
  , unsafeFreezeBitArray
  )
  where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Data.Array.IO
import Data.Array.Unsafe

import Data.BitArray.Immutable

--------------------------------------------------------------------------------

data IOBitArray = IOA 
  { _first :: {-# UNPACK #-} !Int 
  , _last  :: {-# UNPACK #-} !Int 
  , _words :: {-# UNPACK #-} !(IOUArray Int Word64)
  }
  
--------------------------------------------------------------------------------

getBitArrayBounds :: IOBitArray -> IO (Int,Int)
getBitArrayBounds (IOA s t _) = return (s,t)

newBitArray :: (Int,Int) -> Bool -> IO IOBitArray
newBitArray (s,t) b = if t<s
  then error "IOBitArray/newBitArray: empty range"
  else do
    words <- newArray (0,k-1) w
    return (IOA s t words)    
  where
    k = (t-s+64) `shiftR` 6
    w = case b of
      False -> 0
      True  -> 0xFFFFFFFFFFFFFFFF
--               fedcba9876543210       

readBit :: IOBitArray -> Int -> IO Bool
readBit ar@(IOA s t _) j = if j<s || j>t 
  then error "IOBitArray/readBit: index out of range"
  else unsafeReadBit ar j 

unsafeReadBit :: IOBitArray -> Int -> IO Bool
unsafeReadBit (IOA s t a) j = do
  let (k,l) = ind (j-s)
  w <- readArray a k
  return (w `testBit` l)

writeBit :: IOBitArray -> Int -> Bool -> IO ()
writeBit ar@(IOA s t _) j b = if j<s || j>t 
  then error "IOBitArray/writeBit: index out of range"
  else unsafeWriteBit ar j b

unsafeWriteBit :: IOBitArray -> Int -> Bool -> IO ()
unsafeWriteBit (IOA s t a) j b = do
  let (k,l) = ind (j-s)
  w <- readArray a k
  if b 
    then writeArray a k (w `setBit`   l)
    else writeArray a k (w `clearBit` l)  
  return ()

-- | flips the bit and returns the /old/ value
flipBit :: IOBitArray -> Int -> IO Bool
flipBit ar@(IOA s t _) j = if j<s || j>t 
  then error "IOBitArray/flipBit: index out of range"
  else unsafeFlipBit ar j

unsafeFlipBit :: IOBitArray -> Int -> IO Bool
unsafeFlipBit ar@(IOA s t a) j = do
  let (k,l) = ind (j-s)
  w <- readArray a k
  let b = w `testBit` l
  if b
    then writeArray a k (w `clearBit` l)
    else writeArray a k (w `setBit`   l)  
  return b
    
--------------------------------------------------------------------------------
    
thawBitArray :: BitArray -> IO IOBitArray
thawBitArray (A s t x) = 
  thaw x >>= \y -> return (IOA s t y)

unsafeThawBitArray :: BitArray -> IO IOBitArray
unsafeThawBitArray (A s t x) = 
  unsafeThaw x >>= \y -> return (IOA s t y)

freezeBitArray :: IOBitArray -> IO BitArray
freezeBitArray (IOA s t x) = 
  freeze x >>= \y -> return (A s t y)

unsafeFreezeBitArray :: IOBitArray -> IO BitArray
unsafeFreezeBitArray (IOA s t x) = 
  unsafeFreeze x >>= \y -> return (A s t y)

--------------------------------------------------------------------------------

