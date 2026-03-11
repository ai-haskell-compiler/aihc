{- |
Bit manipulation.

Taken from Haskore.

Bit operations work with numbers on the level of ones and zeros.
These functions should be called something like \"pseudo-bit-operations\".
They do not reach into the ones and zeros,
but they do duplicate the effects using regular math.
Note that these bitops, though convenient,
are no more efficient than the high-level arithmetic that does the same thing.
(This is different than in other languages such as C.)
-}

module Sound.MIDI.Bit where

import Data.Maybe.HT (toMaybe, )
import Data.Tuple.HT (swap, )
import Data.Word  (Word8, )
import qualified Data.List as List
import qualified Data.Bits as Bits

{- |
Shift bitwise to the left and right.
-}

shiftL, shiftR :: Bits.Bits a => Int -> a -> a
shiftL = flip Bits.shiftL
shiftR = flip Bits.shiftR

{- |
The call @toBase n x@ takes a given number x and "chops it up,"
returning its digits in base b.  Its output is in the form of a
big-endian list of ints.  divMod is used because it gives the correct
rounding for negative numbers.  Ex. toBytes 1000 -> toBase 256 1000 ->
(256*3) + 232 -> [ 3 , 232 ]
-}

toBase :: Integral a => a -> a -> [a]
toBase b =
   reverse . List.unfoldr (\n -> toMaybe (n>0) (swap (divMod n b)))

toBits, toOctal, toHex, toBytes :: Integral a => a -> [a]
toBytes = toBase 256
toHex   = toBase 16
toOctal = toBase 8
toBits  = toBase 2

{- |
Get only n of the least significant bytes of x.  If it takes less than
n digits to express x, then fill the extra digits with zeros.
-}

someBytes :: Integral a => Int -> a -> [Word8]
someBytes n =
   reverse . take n . map fromIntegral .
   List.unfoldr (Just . swap . flip divMod (2^(8::Int)))

{- |
The fromBase function converts a list of digits in another base into a
single base-10 number.

fromBase b [x,y,z] = x*b^2 + y*b^1 + z*b^0
-}

fromBase :: Integral a => a -> [a] -> a
fromBase base xs = foldl (\a x -> base*a+x) 0 xs

fromBits, fromOctal, fromHex, fromBytes :: Integral a => [a] -> a
fromBytes = fromBase 256
fromHex   = fromBase 16
fromOctal = fromBase 8
fromBits  = fromBase 2


{- |
Like 'replicate' but for big numbers.
It chops the list into blocks of tractable sizes (e.g. @maxBound::Int@).
-}
replicateBig :: Integer -> Integer -> a -> [a]
replicateBig base x c =
   let loopSizes = map fromInteger (toBase base x)
       b = fromInteger base
   in  foldl (\cs n -> concat (replicate b cs) ++ replicate n c) [] loopSizes



{- |
@trunc b n@ takes the b least significant bits of n.
-}

trunc :: Integral a => Int -> a -> a
trunc b n = n `mod` (2^b)

{- |
@splitAt b n@ splits a number into a tuple: (before bit b, after bit b).
-}

splitAt :: Integral a => Int -> a -> (a, a)
splitAt b n = n `divMod` (2^b)
