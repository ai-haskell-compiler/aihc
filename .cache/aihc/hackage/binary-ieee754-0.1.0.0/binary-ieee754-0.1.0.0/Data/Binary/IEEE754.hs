{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-- | This module backports ieee754 double/float combinators from binary 0.8.4 to older
-- version, and simply re-export these combinators for binary >= 0.8.4. You can safely
-- import this module alongside "Data.Binary.Get" and "Data.Binary.Put".
--
--   Implements casting via a 1-elemnt STUArray, as described in
--   <http://stackoverflow.com/a/7002812/263061>.
--
module Data.Binary.IEEE754
  ( -- * Double/Float Word cast
    floatToWord
  , wordToFloat
  , doubleToWord
  , wordToDouble
    -- * Double/Floats Get
  , getFloatbe
  , getFloatle
  , getFloathost
  , getDoublebe
  , getDoublele
  , getDoublehost
    -- * Double/Floats Put
  , putFloatbe
  , putFloatle
  , putFloathost
  , putDoublebe
  , putDoublele
  , putDoublehost
  ) where

import Data.Word (Word32, Word64)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative ((<$>))

------------------------------------------------------------------------
-- Double/Float Word cast

-- | Reinterpret-casts a `Float` to a `Word32`.
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)
{-# INLINE floatToWord #-}

-- | Reinterpret-casts a `Word32` to a `Float`.
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)
{-# INLINE wordToFloat #-}

-- | Reinterpret-casts a `Double` to a `Word64`.
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)
{-# INLINE doubleToWord #-}

-- | Reinterpret-casts a `Word64` to a `Double`.
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)
{-# INLINE wordToDouble #-}

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
{-# INLINE cast #-}

#if !(MIN_VERSION_binary(0,8,4))

------------------------------------------------------------------------
-- Double/Float Get

-- | Read a 'Float' in big endian IEEE-754 format.
getFloatbe :: Get Float
getFloatbe = wordToFloat <$> getWord32be
{-# INLINE getFloatbe #-}

-- | Read a 'Float' in little endian IEEE-754 format.
getFloatle :: Get Float
getFloatle = wordToFloat <$> getWord32le
{-# INLINE getFloatle #-}

-- | Read a 'Float' in IEEE-754 format and host endian.
getFloathost :: Get Float
getFloathost = wordToFloat <$> getWord32host
{-# INLINE getFloathost #-}

-- | Read a 'Double' in big endian IEEE-754 format.
getDoublebe :: Get Double
getDoublebe = wordToDouble <$> getWord64be
{-# INLINE getDoublebe #-}

-- | Read a 'Double' in little endian IEEE-754 format.
getDoublele :: Get Double
getDoublele = wordToDouble <$> getWord64le
{-# INLINE getDoublele #-}

-- | Read a 'Double' in IEEE-754 format and host endian.
getDoublehost :: Get Double
getDoublehost = wordToDouble <$> getWord64host
{-# INLINE getDoublehost #-}

------------------------------------------------------------------------
-- Double/Floats Put

-- | Write a 'Float' in big endian IEEE-754 format.
putFloatbe :: Float -> Put
putFloatbe = putWord32be . floatToWord
{-# INLINE putFloatbe #-}

-- | Write a 'Float' in little endian IEEE-754 format.
putFloatle :: Float -> Put
putFloatle = putWord32le . floatToWord
{-# INLINE putFloatle #-}

-- | Write a 'Float' in native in IEEE-754 format and host endian.
putFloathost :: Float -> Put
putFloathost = putWord32host . floatToWord
{-# INLINE putFloathost #-}

-- | Write a 'Double' in big endian IEEE-754 format.
putDoublebe :: Double -> Put
putDoublebe = putWord64be . doubleToWord
{-# INLINE putDoublebe #-}

-- | Write a 'Double' in little endian IEEE-754 format.
putDoublele :: Double -> Put
putDoublele = putWord64le . doubleToWord
{-# INLINE putDoublele #-}

-- | Write a 'Double' in native in IEEE-754 format and host endian.
putDoublehost :: Double -> Put
putDoublehost = putWord64host . doubleToWord
{-# INLINE putDoublehost #-}

#endif
