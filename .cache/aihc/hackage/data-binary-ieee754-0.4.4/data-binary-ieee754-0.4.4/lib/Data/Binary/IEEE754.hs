-----------------------------------------------------------------------------
-- |
-- Module: Data.Binary.IEEE754
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-----------------------------------------------------------------------------
module Data.Binary.IEEE754 (
	-- * Parsing
	  getFloat16be, getFloat16le
	, getFloat32be, getFloat32le
	, getFloat64be, getFloat64le
	
	-- * Serializing
	, putFloat32be, putFloat32le
	, putFloat64be, putFloat64le
	
	-- * Float <-> Word conversion
	, floatToWord, wordToFloat
	, doubleToWord, wordToDouble
) where

import           Prelude hiding (exp)
import           Data.Bits (shiftL, shiftR, (.|.), (.&.))
import qualified Foreign as F
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

getFloat16be :: G.Get Float
getFloat16be = fmap toFloat16 G.getWord16be

getFloat16le :: G.Get Float
getFloat16le = fmap toFloat16 G.getWord16le

getFloat32be :: G.Get Float
getFloat32be = fmap toFloat G.getWord32be

getFloat32le :: G.Get Float
getFloat32le = fmap toFloat G.getWord32le

getFloat64be :: G.Get Double
getFloat64be = fmap toFloat G.getWord64be

getFloat64le :: G.Get Double
getFloat64le = fmap toFloat G.getWord64le

putFloat32be :: Float -> P.Put
putFloat32be = P.putWord32be . fromFloat

putFloat32le :: Float -> P.Put
putFloat32le = P.putWord32le . fromFloat

putFloat64be :: Double -> P.Put
putFloat64be = P.putWord64be . fromFloat

putFloat64le :: Double -> P.Put
putFloat64le = P.putWord64le . fromFloat

floatToWord :: Float -> F.Word32
floatToWord = fromFloat

wordToFloat :: F.Word32 -> Float
wordToFloat = toFloat

doubleToWord :: Double -> F.Word64
doubleToWord = fromFloat

wordToDouble :: F.Word64 -> Double
wordToDouble = toFloat

toFloat :: (F.Storable word, F.Storable float) => word -> float
toFloat word = unsafePerformIO $ F.alloca $ \buf -> do
	F.poke (F.castPtr buf) word
	F.peek buf

fromFloat :: (F.Storable word, F.Storable float) => float -> word
fromFloat float = unsafePerformIO $ F.alloca $ \buf -> do
	F.poke (F.castPtr buf) float
	F.peek buf

toFloat16 :: F.Word16 -> Float
toFloat16 word16 = toFloat (sign32 .|. word32) where
	sign16 = word16 .&. 0x8000
	exp16 = word16 .&. 0x7C00
	frac16 = word16 .&. 0x3FF
	
	sign32 = if sign16 > 0
		then 0x80000000 -- -0.0
		else 0
	
	word32 :: F.Word32
	word32 | word16 .&. 0x7FFF == 0 = 0
	       | exp16 == 0x7C00 = special
	       | otherwise = shiftL exp32 23 .|. shiftL frac32 13
	
	special = if frac16 == 0
		-- Infinity
		then 0x7F800000
		
		-- NaN; signals are maintained in lower 10 bits
		else 0x7FC00000 .|. fromIntegral frac16
	
	(exp32, frac32) = if exp16 > 0
		then normalised
		else denormalised
	
	normalised = (exp, frac) where
		exp = (fromIntegral exp16 `shiftR` 10) - 15 + 127
		frac = fromIntegral frac16
	
	denormalised = (exp, frac) where
		exp = (fromIntegral exp16 `shiftR` 10) - 15 + 127 - e
		(e, frac ) = step 0 (shiftL frac16 1) where
			step acc x = if x .&. 0x400 == 0
				then step (acc + 1) (shiftL x 1)
				else (acc, fromIntegral x .&. 0x3FF)
