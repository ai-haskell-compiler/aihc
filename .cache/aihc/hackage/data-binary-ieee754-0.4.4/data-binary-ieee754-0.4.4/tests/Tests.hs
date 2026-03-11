-----------------------------------------------------------------------------
-- |
-- Module: Tests
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-----------------------------------------------------------------------------
module Main (tests, main) where

import qualified Data.ByteString.Lazy as B
import           Data.Word (Word8)
import           Data.Binary.Get (Get, runGetState)
import           Data.Binary.Put (Put, runPut)

import          Test.Chell
import          Test.Chell.QuickCheck
import          Test.QuickCheck hiding (label, property)

import          Data.Binary.IEEE754

tests :: [Suite]
tests =
	[ test_Parsing
	, test_Serialising
	, test_Passthrough
	, test_Passthrough_NaN
	]

main :: IO ()
main = Test.Chell.defaultMain tests

test_Parsing :: Suite
test_Parsing = suite "parsing"
	(props_GetFloat16 "16")
	(props_GetFloat32 "32")
	(props_GetFloat64 "64")

test_Serialising :: Suite
test_Serialising = suite "serialising"
	(props_PutFloat32 "32")
	(props_PutFloat64 "64")

test_Passthrough :: Suite
test_Passthrough = suite "passthrough"
	(testPassthrough "32-le" putFloat32le getFloat32le)
	(testPassthrough "32-be" putFloat32be getFloat32be)
	(testPassthrough "64-le" putFloat64le getFloat64le)
	(testPassthrough "64-be" putFloat64be getFloat64be)

test_Passthrough_NaN :: Suite
test_Passthrough_NaN = suite "passthrough-nan"
	(testPassthroughNaN "32-le" putFloat32le getFloat32le)
	(testPassthroughNaN "32-be" putFloat32be getFloat32be)
	(testPassthroughNaN "64-le" putFloat64le getFloat64le)
	(testPassthroughNaN "64-be" putFloat64be getFloat64be)

props_GetFloat16 :: String -> Suite
props_GetFloat16 label =
	let check = checkGet getFloat16be getFloat16le in
	suite label
	
	(check [0, 0]    ((== 0.0) .&& (not . isNegativeZero)))
	(check [0x80, 0] isNegativeZero)
	
	-- Normalised
	(check [0x3C, 0] (==  1.0))
	(check [0xBC, 0] (== -1.0))
	
	-- Denormalised
	(check [0x03, 0xFF] (==  6.097555e-5))
	(check [0x83, 0xFF] (== -6.097555e-5))
	
	-- Infinity
	(check [0x7C, 0] (==  inf32))
	(check [0xFC, 0] (== -inf32))
	
	-- NaN
	(check [0x7E, 0] (isNaN .&& (not . isNegativeNaN)))
	(check [0xFE, 0] isNegativeNaN)

props_GetFloat32 :: String -> Suite
props_GetFloat32 label =
	let check = checkGet getFloat32be getFloat32le in
	suite label
	
	(check [0, 0, 0, 0]    ((== 0.0) .&& (not . isNegativeZero)))
	(check [0x80, 0, 0, 0] isNegativeZero)
	
	-- Normalised
	(check [0x3F, 0x80, 0, 0] (==  1.0))
	(check [0xBF, 0x80, 0, 0] (== -1.0))
	
	-- Denormalised
	(check [0x00, 0x7F, 0xFF, 0xFF] (==  1.1754942106924411e-38))
	(check [0x80, 0x7F, 0xFF, 0xFF] (== -1.1754942106924411e-38))
	
	-- Infinity
	(check [0x7F, 0x80, 0, 0] (==  inf32))
	(check [0xFF, 0x80, 0, 0] (== -inf32))
	
	-- NaN and negative NaN
	(check [0x7F, 0xC0, 0, 0] (isNaN .&& (not . isNegativeNaN)))
	(check [0xFF, 0xC0, 0, 0] isNegativeNaN)

props_GetFloat64 :: String -> Suite
props_GetFloat64 label =
	let check = checkGet getFloat64be getFloat64le in
	suite label
	
	(check [0, 0, 0, 0, 0, 0, 0, 0]    ((== 0.0) .&& (not . isNegativeZero)))
	(check [0x80, 0, 0, 0, 0, 0, 0, 0] isNegativeZero)
	
	-- Normalised
	(check [0x3F, 0xF0, 0, 0, 0, 0, 0, 0] (==  1.0))
	(check [0xBF, 0xF0, 0, 0, 0, 0, 0, 0] (== -1.0))
	
	-- Denormalised
	(check [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (==  2.2250738585072009e-308))
	(check [0x80, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (== -2.2250738585072009e-308))
	
	-- Infinity
	(check [0x7F, 0xF0, 0, 0, 0, 0, 0, 0] (==  inf64))
	(check [0xFF, 0xF0, 0, 0, 0, 0, 0, 0] (== -inf64))
	
	-- NaN
	(check [0x7F, 0xF8, 0, 0, 0, 0, 0, 0] (isNaN .&& (not . isNegativeNaN)))
	(check [0xFF, 0xF8, 0, 0, 0, 0, 0, 0] isNegativeNaN)

props_PutFloat32 :: String -> Suite
props_PutFloat32 label =
	let check = checkPut putFloat32be putFloat32le in
	suite label
	
	(check [0, 0, 0, 0]   0.0)
	(check [0x80, 0, 0, 0] (-0.0))
	
	-- Normalised
	(check [0x3F, 0x80, 0, 0]   1.0)
	(check [0xBF, 0x80, 0, 0] (-1.0))
	
	-- Denormalised
	(check [0x00, 0x7F, 0xFF, 0xFF]   1.1754942106924411e-38)
	(check [0x80, 0x7F, 0xFF, 0xFF] (-1.1754942106924411e-38))
	
	-- Infinity
	(check [0x7F, 0x80, 0, 0]   inf32)
	(check [0xFF, 0x80, 0, 0] (-inf32))
	
	-- NaN
	(check [0x7F, 0xC0, 0, 0]   nan32)
	(check [0xFF, 0xC0, 0, 0] (-nan32))

props_PutFloat64 :: String -> Suite
props_PutFloat64 label =
	let check = checkPut putFloat64be putFloat64le in
	suite label
	
	(check [0, 0, 0, 0, 0, 0, 0, 0]      0.0)
	(check [0x80, 0, 0, 0, 0, 0, 0, 0] (-0.0))
	
	-- Normalised
	(check [0x3F, 0xF0, 0, 0, 0, 0, 0, 0]   1.0)
	(check [0xBF, 0xF0, 0, 0, 0, 0, 0, 0] (-1.0))
	
	-- Denormalised
	(check [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]   2.2250738585072009e-308)
	(check [0x80, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (-2.2250738585072009e-308))
	
	-- Infinity
	(check [0x7F, 0xF0, 0, 0, 0, 0, 0, 0]   inf64)
	(check [0xFF, 0xF0, 0, 0, 0, 0, 0, 0] (-inf64))
	
	-- NaN
	(check [0x7F, 0xF8, 0, 0, 0, 0, 0, 0]   nan64)
	(check [0xFF, 0xF8, 0, 0, 0, 0, 0, 0] (-nan64))

checkGet :: (Show a, Eq a, RealFloat a)
         => Get a -- ^ big endian
         -> Get a -- ^ little endian
         -> [Word8] -- ^ big-endian bytes
         -> (a -> Bool) -- ^ verify result
         -> Test
checkGet getBE getLE bytes f = property "get" $ forAll (return bytes) (const valid) where
	valid = B.null remainingBE && B.null remainingLE && f xBE && f xLE
	(xBE, remainingBE, _) = runGetState getBE (B.pack bytes) 0
	(xLE, remainingLE, _) = runGetState getLE (B.pack (reverse bytes)) 0

checkPut :: Show a
         => (a -> Put) -- ^ big endian
         -> (a -> Put) -- ^ little endian
         -> [Word8] -- ^ expected big-endian bytes
         -> a
         -> Test
checkPut putBE putLE bytes x = property "put" $ forAll (return x) (const valid) where
	valid = sameResult && bytes == B.unpack bytesBE
	sameResult = bytesBE == B.reverse bytesLE
	bytesBE = runPut (putBE x)
	bytesLE = runPut (putLE x)

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) f g x = f x && g x

isNegativeNaN :: RealFloat a => a -> Bool
isNegativeNaN x = isNaN x && frac < 0 where
	(frac, _) = decodeFloat x

-- Verify that the given put and get functions are inverses.
testPassthrough :: (Arbitrary a, Show a, Eq a)
                => String
                -> (a -> Put)
                -> Get a
                -> Test
testPassthrough name put get = property name $ \x -> let
	bytes = runPut (put x)
	(x', remaining, _) = runGetState get bytes 0
	in x == x' && B.null remaining

testPassthroughNaN :: (Arbitrary a, RealFloat a, Read a)
                    => String
                    -> (a -> Put)
                    -> Get a
                    -> Test
testPassthroughNaN name put get = property name valid where
	nan = read "NaN"
	check x = decodeFloat x == decodeFloat x' && B.null remaining where
		bytes = runPut (put x)
		(x', remaining, _) = runGetState get bytes 0
	valid = check nan && check (- nan)

-- Pseudo-literals for special values
inf32 :: Float
inf32 = read "Infinity"

inf64 :: Double
inf64 = read "Infinity"

nan32 :: Float
nan32 = - (read "NaN")

nan64 :: Double
nan64 = - (read "NaN")
