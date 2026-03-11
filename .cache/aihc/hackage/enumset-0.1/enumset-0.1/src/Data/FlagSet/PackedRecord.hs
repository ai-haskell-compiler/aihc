module Data.FlagSet.PackedRecord (
   getIntByMask, putIntByMask, accessorIntByMask,
   getIntByRange, putIntByRange, accessorIntByRange,
   ) where

import Data.FlagSet (T(Cons), Mask(Mask), )

import qualified Data.Bits as B
import Data.Bits (Bits, (.&.), (.|.), )

import qualified Data.Accessor.Basic as Acc

import Data.EnumBitSet.Utility ((.-.), )


leastSigBit :: (Num w, Bits w) => w -> w
leastSigBit m = (-m) .&. m

getIntByMask ::
   (Bits w, Integral w, Integral i) =>
   Mask w a b -> T w a -> i
getIntByMask (Mask m) (Cons fs) =
   -- I hope that the division is converted to a shift
   fromIntegral $ div (m .&. fs) (leastSigBit m)

putIntByMask ::
   (Bits w, Integral w, Integral i) =>
   Mask w a b -> i -> T w a -> T w a
putIntByMask (Mask m) i (Cons fs) =
   Cons $ (fs .-. m) .|. (fromIntegral i * leastSigBit m)

accessorIntByMask ::
   (Bits w, Integral w, Integral i) =>
   Mask w a b -> Acc.T (T w a) i
accessorIntByMask m =
   Acc.fromSetGet (putIntByMask m) (getIntByMask m)


maskFromNumber ::
   (Num w, Bits w) =>
   Int -> w
maskFromNumber number =
   B.shiftL 1 number - 1

getIntByRange ::
   (Bits w, Integral w, Integral i) =>
   Int -> Int -> T w a -> i
getIntByRange number start (Cons fs) =
   fromIntegral $ B.shiftR fs start .&. maskFromNumber number

putIntByRange ::
   (Bits w, Integral w, Integral i) =>
   Int -> Int -> i -> T w a -> T w a
putIntByRange number start i (Cons fs) =
   Cons $
      (fs .-. B.shiftL (maskFromNumber number) start)
      .|.
      B.shiftL (fromIntegral i) start

accessorIntByRange ::
   (Bits w, Integral w, Integral i) =>
   Int -> Int -> Acc.T (T w a) i
accessorIntByRange number start =
   Acc.fromSetGet (putIntByRange number start) (getIntByRange number start)
