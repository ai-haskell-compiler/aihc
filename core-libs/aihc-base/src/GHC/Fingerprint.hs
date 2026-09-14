-- | Fingerprints of byte strings, of text and of other fingerprints.
--
-- GHC computes these with MD5. aihc uses FNV-1a instead, run as two lanes
-- from two different offset bases to fill the two words of a
-- 'Fingerprint'. Nothing aihc produces is compared against a fingerprint
-- that GHC computed, and nothing persists one across toolchains, so the
-- values only have to be spread well over distinct inputs. They are not
-- claimed to match GHC's, and they are not collision resistant against an
-- adversary: a caller that needs a decision rather than a hash bucket --
-- 'Type.Reflection.eqTypeRep', for one -- compares structure instead.
module GHC.Fingerprint
  ( Fingerprint (..),
    fingerprint0,
    fingerprintData,
    fingerprintString,
    fingerprintFingerprints,
  )
where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (ord)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import GHC.Fingerprint.Type (Fingerprint (..))
import GHC.List (foldl')
import GHC.Word (Word64, Word8)
import Prelude

-- | The fingerprint GHC uses as a unit for combining. It is not the
-- fingerprint of the empty input.
fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

-- | The fingerprint of the UTF-8 encoding of a string.
fingerprintString :: String -> Fingerprint
fingerprintString = foldl' mixChar start

-- | The fingerprint of a block of memory.
fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData address size = go start 0
  where
    go accumulator offset =
      if offset >= size
        then pure accumulator
        else do
          byte <- peekByteOff address offset
          go (mixByte accumulator byte) (offset + 1)

-- | The fingerprint of a sequence of fingerprints. Each one contributes
-- its sixteen bytes, least significant first, so the sequence determines
-- the result and two sequences of different lengths cannot agree.
fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints = foldl' mixFingerprint start

-- | The two FNV-1a states, carried in a 'Fingerprint' so that a fold has
-- nothing else to allocate. FNV-1a leaves its state as the digest, so an
-- accumulator needs no finishing step.
start :: Fingerprint
start = Fingerprint offsetBasis (offsetBasis `xor` goldenRatio)

mixByte :: Fingerprint -> Word8 -> Fingerprint
mixByte (Fingerprint firstLane secondLane) byte =
  Fingerprint (step firstLane) (step secondLane)
  where
    value = fromIntegral byte
    step lane = (lane `xor` value) * prime

mixWord64 :: Fingerprint -> Word64 -> Fingerprint
mixWord64 accumulator value = foldl' mixShifted accumulator [0, 8, 16, 24, 32, 40, 48, 56]
  where
    mixShifted state shift = mixByte state (fromIntegral (value `shiftR` shift))

mixFingerprint :: Fingerprint -> Fingerprint -> Fingerprint
mixFingerprint accumulator (Fingerprint firstWord secondWord) =
  mixWord64 (mixWord64 accumulator firstWord) secondWord

-- | A character contributes its UTF-8 encoding, so that a string and the
-- block of memory holding its UTF-8 bytes fingerprint alike.
mixChar :: Fingerprint -> Char -> Fingerprint
mixChar accumulator character
  | point < 0x80 = mixBytes [point]
  | point < 0x800 = mixBytes [0xc0 .|. shiftR point 6, continuation 0]
  | point < 0x10000 = mixBytes [0xe0 .|. shiftR point 12, continuation 6, continuation 0]
  | otherwise = mixBytes [0xf0 .|. shiftR point 18, continuation 12, continuation 6, continuation 0]
  where
    point = ord character
    continuation shift = 0x80 .|. (shiftR point shift .&. 0x3f)
    mixBytes = foldl' mixByte accumulator . map fromIntegral

-- | The 64-bit FNV-1a offset basis, @0xcbf29ce484222325@, spelled in two
-- halves because it does not fit in an 'Int' literal.
offsetBasis :: Word64
offsetBasis = shiftL 0xcbf29ce4 32 .|. 0x84222325

-- | The fractional part of the golden ratio, which separates the second
-- lane's basis from the first.
goldenRatio :: Word64
goldenRatio = shiftL 0x9e3779b9 32 .|. 0x7f4a7c15

-- | The 64-bit FNV prime, @2^40 + 2^8 + 0xb3@.
prime :: Word64
prime = 0x100000001b3
