
module Data.BitArray.Immutable where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Data.Array.Unboxed

--------------------------------------------------------------------------------

-- | A packed bit array. 
-- Internally, it is represented as an unboxed array of 'Word64'-s.
data BitArray = A 
  { _first :: {-# UNPACK #-} !Int 
  , _last  :: {-# UNPACK #-} !Int 
  , _words :: {-# UNPACK #-} !(UArray Int Word64)
  }
  
--------------------------------------------------------------------------------

ind :: Int -> (Int,Int)
ind i = (k,l) where
  k = i `shiftR` 6
  l = i - k `shiftL` 6

--------------------------------------------------------------------------------

{-# SPECIALIZE intToBool :: Int -> Bool #-}
intToBool :: Integral a => a -> Bool
intToBool n = case n of
  0 -> False
  _ -> True
  
{-# SPECIALIZE boolToInt :: Bool -> Int #-}
boolToInt :: Integral a => Bool -> a
boolToInt b = case b of
  False -> 0
  True  -> 1

--------------------------------------------------------------------------------
