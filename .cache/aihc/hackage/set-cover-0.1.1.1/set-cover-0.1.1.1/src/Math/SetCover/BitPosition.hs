module Math.SetCover.BitPosition
         (C, Sized, unpack, singleton, bitPosition) where

import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import Math.SetCover.Bit ((.&.))

import qualified Data.IntSet as IntSet; import Data.IntSet (IntSet)
import qualified Data.Bits as Bits
import Data.Bits (Bits, shiftR, complement)
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Maybe.HT (toMaybe)


unpackGen :: (C bits) => BitSet.Set bits -> [Int]
unpackGen = map bitPosition . decompose

decompose :: (Bit.C bits) => BitSet.Set bits -> [BitSet.Set bits]
decompose =
   List.unfoldr $ \set ->
      toMaybe (not $ BitSet.null set) $
         let x = BitSet.keepMinimum set
         in  (x, BitSet.difference set x)

{-# INLINE positionMasks #-}
positionMasks :: (Integral bits, Bits bits, Bit.C bits) => [bits]
positionMasks =
   map (complement . div (-1) . (1+)) $
   takeWhile (/=0) $ iterate (\w -> w*w) 2

{-
Alternative: @bits-extras:Data.Bits.Extras.lowestBitPlus1@
-}
{-# INLINE bitPositionGen #-}
bitPositionGen ::
   (Integral bits, Bits bits, Bit.C bits) => [bits] -> bits -> Int
bitPositionGen masks w =
   foldr
      (\mask acc -> fromEnum (mask .&. w /= Bit.empty) + 2*acc)
      0 masks

class Bit.C bits => C bits where
   bit :: Int -> bits
   bitPositionPlain :: bits -> Int
   unpack :: BitSet.Set bits -> [Int]

instance C Word8 where
   bit = Bits.bit
   bitPositionPlain = bitPositionGen positionMasks
   unpack = unpackGen

instance C Word16 where
   bit = Bits.bit
   bitPositionPlain = bitPositionGen positionMasks
   unpack = unpackGen

instance C Word32 where
   bit = Bits.bit
   bitPositionPlain = bitPositionGen positionMasks
   unpack = unpackGen

instance C Word64 where
   bit = Bits.bit
   bitPositionPlain = bitPositionGen positionMasks
   unpack = unpackGen

instance C Integer where
   bit = Bits.bit
   bitPositionPlain =
      ListHT.switchR
         (error "bitPosition: zero Integer")
         (\_ (offset,x) -> offset + bitPositionPlain (word64 x)) .
      zip [0, 64 ..] . takeWhile (/=0) . iterate (flip shiftR 64)
   unpack =
      concatMap (\(offset,x) -> map (offset+) $ unpack (BitSet.Set x)) .
      zip [0, 64 ..] .
      map (\w -> word64 $ w .&. fromIntegral (complement 0 :: Word64)) .
      takeWhile (/=0) . iterate (flip shiftR 64) . BitSet.getBits

instance C IntSet where
   bit = IntSet.singleton
   bitPositionPlain = IntSet.findMin
   unpack = IntSet.toList . BitSet.getBits

word64 :: Integer -> Word64
word64 = fromIntegral


newtype Size bits = Size Int

class C bits => Sized bits where size :: Size bits
instance Sized Word8  where size = Size 8
instance Sized Word16 where size = Size 16
instance Sized Word32 where size = Size 32
instance Sized Word64 where size = Size 64

instance (Sized a, C b) => C (Bit.Sum a b) where
   bit = bitSum size
   bitPositionPlain = bitSumPosition size
   unpack = bitSumUnpack size

bitSum :: (C a, C b) => Size a -> Int -> Bit.Sum a b
bitSum (Size offset) pos =
   if pos < offset
     then Bit.Sum (bit pos) Bit.empty
     else Bit.Sum Bit.empty (bit $ pos-offset)

bitSumPosition :: (C a, C b) => Size a -> Bit.Sum a b -> Int
bitSumPosition (Size offset) (Bit.Sum a b) =
   if a == Bit.empty
     then offset + bitPositionPlain b
     else bitPositionPlain a

bitSumUnpack :: (C a, C b) => Size a -> BitSet.Set (Bit.Sum a b) -> [Int]
bitSumUnpack (Size offset) (BitSet.Set (Bit.Sum a b)) =
   unpack (BitSet.Set a) ++ map (offset +) (unpack (BitSet.Set b))

bitPosition :: (C bits) => BitSet.Set bits -> Int
bitPosition = bitPositionPlain . BitSet.getBits

singleton :: (C bits) => Int -> BitSet.Set bits
singleton = BitSet.Set . bit
