{- |
Parse primitive types contained in MIDI files.
-}
module Sound.MIDI.Parser.Primitive
   (getByte,
    getN, getString, getBigN, getNByteInt,
    get1, get2, get3, get4,
    getNByteCardinal,
    getVar, getVarBytes,
    getEnum, makeEnum, ) where

import qualified Sound.MIDI.Parser.Class as Parser
import Control.Monad (replicateM, liftM, )

import Sound.MIDI.IO (ByteList, listCharFromByte, )
import qualified Sound.MIDI.Bit as Bit
import Data.Bits (testBit, clearBit)
import Data.Word (Word8)
import qualified Numeric.NonNegative.Wrapper as NonNeg



{- |
'getByte' gets a single byte from the input.
-}
getByte :: Parser.C parser => Parser.Fragile parser Word8
getByte = Parser.getByte


{- |
@getN n@ returns n characters (bytes) from the input.
-}
getN :: Parser.C parser => NonNeg.Int -> Parser.Fragile parser ByteList
getN n = replicateM (NonNeg.toNumber n) getByte

getString :: Parser.C parser => NonNeg.Integer -> Parser.Fragile parser String
getString n = liftM listCharFromByte (getBigN n)

getBigN :: Parser.C parser => NonNeg.Integer -> Parser.Fragile parser ByteList
getBigN n =
   sequence $
   Bit.replicateBig
      (1 + fromIntegral (maxBound :: NonNeg.Int))
      (NonNeg.toNumber n)
      getByte


{- |
'get1', 'get2', 'get3', and 'get4' take 1-, 2-, 3-, or
4-byte numbers from the input (respectively), convert the base-256 data
into a single number, and return.
-}
get1 :: Parser.C parser => Parser.Fragile parser Int
get1 = liftM fromIntegral getByte

getNByteInt :: Parser.C parser => NonNeg.Int -> Parser.Fragile parser Int
getNByteInt n =
   liftM Bit.fromBytes (replicateM (NonNeg.toNumber n) get1)

get2, get3, get4 :: Parser.C parser => Parser.Fragile parser Int
get2 = getNByteInt 2
get3 = getNByteInt 3
get4 = getNByteInt 4

getByteAsCardinal :: Parser.C parser => Parser.Fragile parser NonNeg.Integer
getByteAsCardinal = liftM fromIntegral getByte

getNByteCardinal :: Parser.C parser => NonNeg.Int -> Parser.Fragile parser NonNeg.Integer
getNByteCardinal n =
   liftM Bit.fromBytes (replicateM (NonNeg.toNumber n) getByteAsCardinal)

{- |
/Variable-length quantities/ are used often in MIDI notation.
They are represented in the following way:
Each byte (containing 8 bits) uses the 7 least significant bits to store information.
The most significant bit is used to signal whether or not more information is coming.
If it's @1@, another byte is coming.
If it's @0@, that byte is the last one.
'getVar' gets a variable-length quantity from the input.
-}
getVar :: Parser.C parser => Parser.Fragile parser NonNeg.Integer
getVar =
   liftM (Bit.fromBase (2^(7::Int)) . map fromIntegral) getVarBytes

{- |
The returned list contains only bytes with the most significant bit cleared.
These are digits of a 128-ary number.
-}
getVarBytes :: Parser.C parser => Parser.Fragile parser [Word8]
getVarBytes =
   do
      digit <- getByte
      if flip testBit 7 digit            -- if it's the last byte
        then liftM (flip clearBit 7 digit :) getVarBytes
        else return [digit]


getEnum :: (Parser.C parser, Enum enum, Bounded enum) => Parser.Fragile parser enum
getEnum = makeEnum =<< get1

makeEnum :: (Parser.C parser, Enum enum, Bounded enum) => Int -> Parser.Fragile parser enum
makeEnum n =
   let go :: (Parser.C parser, Enum a) => a -> a -> Parser.Fragile parser a
       go lower upper =
          if fromEnum lower <= n && n <= fromEnum upper
            then return (toEnum n)
            else Parser.giveUp ("value " ++ show n ++ " is out of range for enumeration")
   in  go minBound maxBound
