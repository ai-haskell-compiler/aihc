module Combinatorics.Battleship.Count.Counter (
   C,
   Composed,
   zero,
   one,
   add,
   sum,
   toInteger,
   propAdd,
   ) where

import Control.Monad (liftM2, )

import qualified Data.List as List
import Data.Bits (shiftL, )
import Data.Word (Word8, Word32, Word64, )

import Foreign.Storable
          (Storable, sizeOf, alignment,
           poke, peek, pokeByteOff, peekByteOff, )

import qualified Test.QuickCheck as QC

import Prelude hiding (sum, toInteger, )


class C a where
   zero, one :: a
   add :: a -> a -> a

class (C a, Ord a) => Integ a where
   toInteger :: a -> Integer
   rangeSize :: a -> Integer

instance C Word8 where
   zero = 0; one = 1
   add = (+)

instance Integ Word8 where
   toInteger = fromIntegral
   rangeSize _ = shiftL 1 8

instance C Word32 where
   zero = 0; one = 1
   add = (+)

instance Integ Word32 where
   toInteger = fromIntegral
   rangeSize _ = shiftL 1 32

instance C Word64 where
   zero = 0; one = 1
   add = (+)

instance Integ Word64 where
   toInteger = fromIntegral
   rangeSize _ = shiftL 1 64

sum :: (C a) => [a] -> a
sum = List.foldl' add zero

data Composed hi lo = Composed !hi !lo
   deriving (Eq, Ord)

instance (C hi, C lo, Ord lo) => C (Composed hi lo) where
   zero = Composed zero zero
   one = Composed zero one
   add (Composed xh xl) (Composed yh yl) =
      let zh = add xh yh; zl = add xl yl
      in  Composed (if zl < xl then add zh one else zh) zl

instance (Integ hi, Integ lo) => Integ (Composed hi lo) where
   rangeSize ~(Composed hi lo) = rangeSize hi * rangeSize lo
   toInteger (Composed hi lo) =
      toInteger hi * rangeSize lo + toInteger lo

instance (Integ hi, Integ lo) => Show (Composed hi lo) where
   show = show . toInteger

-- | This instance expects that there is no need for padding for alignment
instance (Storable a, Storable b) => Storable (Composed a b) where
   sizeOf ~(Composed a b) = sizeOf a + sizeOf b
   alignment ~(Composed a b) = alignment a `lcm` alignment b
   poke ptr (Composed a b) = do
      pokeByteOff ptr 0 a
      pokeByteOff ptr (sizeOf a) b
   peek ptr = do
      a <- peekByteOff ptr 0
      b <- peekByteOff ptr (sizeOf a)
      return $ Composed a b


instance (QC.Arbitrary a, QC.Arbitrary b) => QC.Arbitrary (Composed a b) where
   arbitrary = liftM2 Composed QC.arbitrary QC.arbitrary
   shrink (Composed hi lo) = map (uncurry Composed) $ QC.shrink (hi,lo)

propAdd ::
   Composed (Composed Word64 Word32) (Composed Word32 Word32) ->
   Composed (Composed Word64 Word32) (Composed Word32 Word32) ->
   Bool
propAdd a b =
   toInteger (add a b) == mod (toInteger a + toInteger b) (rangeSize a)
