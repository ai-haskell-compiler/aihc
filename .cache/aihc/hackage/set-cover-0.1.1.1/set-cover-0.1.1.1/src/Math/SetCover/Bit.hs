module Math.SetCover.Bit where

import qualified Data.IntSet as IntSet; import Data.IntSet (IntSet)
import qualified Data.Bits as Bits
import Data.Bits (Bits, complement)
import Data.Word (Word8, Word16, Word32, Word64)
import Prelude hiding (null)


infixl 7 .&.
infixl 5 .|.

{- |
This class is similar to the 'Bits' class from the @base@ package
but adds 'keepMinimum' and misses the rotation stuff.
-}
class Ord bits => C bits where
   empty :: bits
   keepMinimum :: bits -> bits
   difference, xor, (.&.), (.|.) :: bits -> bits -> bits

instance C Word8 where
   empty = 0
   difference xs ys = xs .&. complement ys
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)

instance C Word16 where
   empty = 0
   difference xs ys = xs .&. complement ys
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)

instance C Word32 where
   empty = 0
   difference xs ys = xs .&. complement ys
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)

instance C Word64 where
   empty = 0
   difference xs ys = xs .&. complement ys
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)

instance C Integer where
   empty = 0
   difference xs ys = xs .&. complement ys
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)

instance C IntSet where
   empty = IntSet.empty
   difference = IntSet.difference
   keepMinimum = IntSet.singleton . IntSet.findMin
   xor x y = IntSet.difference (IntSet.union x y) (IntSet.intersection x y)
   (.&.) = IntSet.intersection
   (.|.) = IntSet.union


{-
cf. package largeword
-}
data Sum a b = Sum !a !b
   deriving (Eq, Ord, Show)

instance (C a, C b) => C (Sum a b) where
   empty = Sum empty empty
   difference (Sum xl xh) (Sum yl yh) =
      Sum (difference xl yl) (difference xh yh)
   xor (Sum xl xh) (Sum yl yh) = Sum (xor xl yl) (xor xh yh)
   Sum xl xh .&. Sum yl yh = Sum (xl.&.yl) (xh.&.yh)
   Sum xl xh .|. Sum yl yh = Sum (xl.|.yl) (xh.|.yh)
   keepMinimum (Sum l h) =
      if l == empty
        then Sum empty (keepMinimum h)
        else Sum (keepMinimum l) empty

bitLeft :: (Bits a, C b) => Int -> Sum a b
bitLeft n = Sum (Bits.bit n) empty

bitRight :: (C a, Bits b) => Int -> Sum a b
bitRight n = Sum empty (Bits.bit n)
