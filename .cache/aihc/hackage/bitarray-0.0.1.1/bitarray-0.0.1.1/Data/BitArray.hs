
-- | Immutable one-dimensional packed bit arrays.
-- The main advantage should be compactness in memory.

module Data.BitArray 
  ( BitArray
  , bitArrayBounds
  , lookupBit
  , unsafeLookupBit
  -- * Bit array construction \/ deconstruction
  , bitArray
  , bitArray'
  , accumBitArray
  , listBitArray
  , bits
  -- * 0\/1 versions
  , bits01
  , listBitArray01
  ) 
  where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.ST

import Data.Bits
import Data.Word

import Data.Array.Unboxed

import Data.BitArray.Immutable
import Data.BitArray.ST

--------------------------------------------------------------------------------

instance Eq BitArray where
  ar1 == ar2 = bits ar1 == bits ar2

instance Ord BitArray where
  compare ar1 ar2 = compare (bits ar1) (bits ar2)

instance Show BitArray where 
  show ar@(A s t a) = "listBitArray01 " ++ show (s,t) ++ " " ++ show (bits01 ar)
  
--------------------------------------------------------------------------------

bitArrayBounds :: BitArray -> (Int,Int)
bitArrayBounds (A s t _) = (s,t)

lookupBit :: BitArray -> Int -> Bool
lookupBit ar@(A s t _) j = if j<s || j>t 
  then error "BitArray/lookupBit: index out of range"
  else unsafeLookupBit ar j
  
unsafeLookupBit :: BitArray -> Int -> Bool
unsafeLookupBit (A s t a) j = testBit w l where
  (k,l) = ind (j-s) 
  w = a!k 

--------------------------------------------------------------------------------

-- | Unspecified values become 'False'.
bitArray :: (Int,Int) -> [(Int,Bool)] -> BitArray
bitArray = accumBitArray (flip const) False 

-- | The first argument gives the default value (instead of 'False')
bitArray' :: Bool -> (Int,Int) -> [(Int,Bool)] -> BitArray
bitArray' = accumBitArray (flip const)

{-# SPECIALIZE accumBitArray :: (Bool -> Bool -> Bool) -> Bool -> (Int,Int) -> [(Int,Bool)] -> BitArray #-}
accumBitArray :: (Bool -> a -> Bool) -> Bool -> (Int,Int) -> [(Int,a)] -> BitArray
accumBitArray f e st xs = runST $ do
  ar <- newBitArray st e
  forM_ xs $ \(i,x) -> do
    b <- readBit ar i
    writeBit ar i (f b x)
  unsafeFreezeBitArray ar
    
-- | If the list is too short, the rest of the array is filled with 'False'.
listBitArray :: (Int,Int) -> [Bool] -> BitArray
listBitArray (s,t) bs = A s t a where
  a = listArray (0,k-1) chunks
  k = (t-s+64) `shiftR` 6
  chunks = take k $ worker (bs ++ repeat False)
  worker bs = convert (take 64 bs) : worker (drop 64 bs)
  convert bs = fst $ foldl f (0,1) bs
  f (x,e) b = if b then (x+e, e+e) else (x, e+e)   

bits :: BitArray -> [Bool]
bits (A s t a) = take (t-s+1) $ concatMap worker (elems a) where
  worker i = fst $ foldl f ([], i) [(0::Int)..63]
  f (bs,i) _ = ( (0 /= i .&. 0x8000000000000000) : bs, shiftL i 1)

--------------------------------------------------------------------------------

listBitArray01 :: (Int,Int) -> [Int] -> BitArray
listBitArray01 st is = listBitArray st (map intToBool is)

bits01 :: BitArray -> [Int]
bits01 = map boolToInt . bits
 
--------------------------------------------------------------------------------
