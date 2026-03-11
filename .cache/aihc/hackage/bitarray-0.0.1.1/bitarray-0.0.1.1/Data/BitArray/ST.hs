
-- | Mutable one-dimensional packed bit arrays in the (strict) ST monad.

module Data.BitArray.ST 
  ( STBitArray 
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

import Control.Monad.ST

import Data.Word
import Data.Bits

import Data.Array.ST
import Data.Array.Unsafe

import Data.BitArray.Immutable

--------------------------------------------------------------------------------

data STBitArray s = STA 
  { _first :: {-# UNPACK #-} !Int 
  , _last  :: {-# UNPACK #-} !Int 
  , _words :: {-# UNPACK #-} !(STUArray s Int Word64)
  }
  
--------------------------------------------------------------------------------

getBitArrayBounds :: STBitArray s -> ST s (Int,Int)
getBitArrayBounds (STA s t _) = return (s,t)

newBitArray :: (Int,Int) -> Bool -> ST s (STBitArray s)
newBitArray (s,t) b = if t<s
  then error "STBitArray/newBitArray: empty range"
  else do
    words <- newArray (0,k-1) w
    return (STA s t words)    
  where
    k = (t-s+64) `shiftR` 6
    w = case b of
      False -> 0
      True  -> 0xFFFFFFFFFFFFFFFF
--               fedcba9876543210       

readBit :: STBitArray s -> Int -> ST s Bool
readBit ar@(STA s t _) j = if j<s || j>t 
  then error "STBitArray/readBit: index out of range"
  else unsafeReadBit ar j 

unsafeReadBit :: STBitArray s -> Int -> ST s Bool
unsafeReadBit (STA s t a) j = do
  let (k,l) = ind (j-s)
  w <- readArray a k
  return (w `testBit` l)

writeBit :: STBitArray s -> Int -> Bool -> ST s ()
writeBit ar@(STA s t _) j b = if j<s || j>t 
  then error "STBitArray/writeBit: index out of range"
  else unsafeWriteBit ar j b

unsafeWriteBit :: STBitArray s -> Int -> Bool -> ST s ()
unsafeWriteBit (STA s t a) j b = do
  let (k,l) = ind (j-s)
  w <- readArray a k
  if b 
    then writeArray a k (w `setBit`   l)
    else writeArray a k (w `clearBit` l)  
  return ()

-- | flips the bit and returns the /old/ value
flipBit :: STBitArray s -> Int -> ST s Bool
flipBit ar@(STA s t _) j = if j<s || j>t 
  then error "STBitArray/flipBit: index out of range"
  else unsafeFlipBit ar j

unsafeFlipBit :: STBitArray s -> Int -> ST s Bool
unsafeFlipBit ar@(STA s t a) j = do
  let (k,l) = ind (j-s)
  w <- readArray a k
  let b = w `testBit` l
  if b
    then writeArray a k (w `clearBit` l)
    else writeArray a k (w `setBit`   l)  
  return b
    
--------------------------------------------------------------------------------
    
thawBitArray :: BitArray -> ST s (STBitArray s)
thawBitArray (A s t x) = 
  thaw x >>= \y -> return (STA s t y)

unsafeThawBitArray :: BitArray -> ST s (STBitArray s)
unsafeThawBitArray (A s t x) = 
  unsafeThaw x >>= \y -> return (STA s t y)

freezeBitArray :: STBitArray s -> ST s BitArray
freezeBitArray (STA s t x) = 
  freeze x >>= \y -> return (A s t y)

unsafeFreezeBitArray :: STBitArray s -> ST s BitArray
unsafeFreezeBitArray (STA s t x) = 
  unsafeFreeze x >>= \y -> return (A s t y)

--------------------------------------------------------------------------------
