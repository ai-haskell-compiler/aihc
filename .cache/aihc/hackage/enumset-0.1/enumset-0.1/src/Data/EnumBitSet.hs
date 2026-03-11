{- |
Similar to Data.Edison.Coll.EnumSet
but it allows to choose the underlying type for bit storage.
This is really a low-level module for type-safe foreign function interfaces.

The integer representation of the enumeration type
is the bit position of the flag within the bitvector.
-}
module Data.EnumBitSet (
   T(Cons, decons),
   fromEnum,
   fromEnums,
   toEnums,
   intToEnums,
   mostSignificantPosition,
   singletonByPosition,
   null,
   empty,
   singleton,
   disjoint,
   subset,
   (.&.),
   (.-.),
   (.|.),
   xor,
   unions,
   get,
   put,
   accessor,
   set,
   clear,
   flip,
   fromBool,
   ) where

import qualified Data.EnumBitSet.Utility as U
import qualified Data.Bits as B
import Data.Bits (Bits, )

import Data.Monoid (Monoid(mempty, mappend), )
import Data.Semigroup (Semigroup((<>)), )

import qualified Foreign.Storable.Newtype as Store
import Foreign.Storable (Storable(sizeOf, alignment, peek, poke), )

import qualified Data.Accessor.Basic as Acc

import qualified Prelude as P
import Prelude hiding (fromEnum, toEnum, null, flip, )


newtype T word index = Cons {decons :: word}
   deriving (Eq)

instance (Enum a, Storable w) => Storable (T w a) where
   sizeOf = Store.sizeOf decons
   alignment = Store.alignment decons
   peek = Store.peek Cons
   poke = Store.poke decons


instance (Enum a, Bits w) => Semigroup (T w a) where
   (<>) = (.|.)

{- |
Since this data type is intended for constructing flags,
we choose the set union as 'mappend'.
For intersection we would also not have a canonical identity element.
-}
instance (Enum a, Bits w) => Monoid (T w a) where
   mempty = empty
   mappend = (.|.)


fromEnum :: (Enum a, Bits w) => a -> T w a
fromEnum = Cons . B.bit . P.fromEnum

fromEnums :: (Enum a, Bits w) => [a] -> T w a
fromEnums = Cons . foldl B.setBit U.empty . map P.fromEnum

toEnums :: (Enum a, Bits w) => T w a -> [a]
toEnums =
   map fst . filter (P.flip B.testBit 0 . snd) .
   zip [P.toEnum 0 ..] .
   takeWhile (U.empty /= ) . iterate (P.flip B.shiftR 1) .
   decons

intToEnums :: (Enum a, Integral w) => T w a -> [a]
intToEnums =
   map fst . filter (odd . snd) .
   zip [P.toEnum 0 ..] .
   takeWhile (0/=) . iterate (P.flip div 2) .
   decons


{- |
floor of binary logarithm -
Intended for getting the position of a single set bit.
This in turn is intended for implementing an 'Enum' instance
if you only know masks but no bit positions.
-}
{-# INLINE mostSignificantPosition #-}
mostSignificantPosition :: (Bits w, Storable w) => T w a -> Int
mostSignificantPosition (Cons x) =
   snd $
   foldl
      (\(x0,pos) testPos ->
         let x1 = B.shiftR x0 testPos
         in  if x1 == U.empty
               then (x0, pos)
               else (x1, pos+testPos))
      (x,0) $
   reverse $
   takeWhile (< sizeOf x * 8) $
   iterate (2*) 1

{- |
set a bit -
Intended for implementing an 'Enum' instance
if you only know masks but no bit positions.
-}
{-# INLINE singletonByPosition #-}
singletonByPosition :: (Bits w) => Int -> T w a
singletonByPosition = Cons . B.setBit U.empty


null :: (Enum a, Bits w) => T w a -> Bool
null (Cons x)  =  x==U.empty

empty :: (Enum a, Bits w) => T w a
empty = Cons U.empty

disjoint :: (Enum a, Bits w) => T w a -> T w a -> Bool
disjoint x y = null (x .&. y)

{- |
@subset a b@ is 'True' if @a@ is a subset of @b@.
-}
subset :: (Enum a, Bits w) => T w a -> T w a -> Bool
subset x y = null (x .-. y)


{-# INLINE lift2 #-}
lift2 :: (w -> w -> w) -> (T w a -> T w a -> T w a)
lift2 f (Cons x) (Cons y) = Cons (f x y)

-- fixities like in Data.Bits
infixl 7 .&., .-.
infixl 5 .|.

(.&.), (.-.), (.|.), xor :: (Enum a, Bits w) => T w a -> T w a -> T w a
(.&.) = lift2 (B..&.)
(.|.) = lift2 (B..|.)
(.-.) = lift2 (\x y -> x B..&. B.complement y)
xor   = lift2 B.xor

unions :: (Enum a, Bits w) => [T w a] -> T w a
unions = foldl (.|.) empty


-- | could also be named @member@ like in @Set@ or @elem@ as in '[]'
get :: (Enum a, Bits w) => a -> T w a -> Bool
get n = P.flip B.testBit (P.fromEnum n) . decons

put :: (Enum a, Bits w) => a -> Bool -> T w a -> T w a
put n b s =
   fromBool n b .|. clear n s

accessor :: (Enum a, Bits w) => a -> Acc.T (T w a) Bool
accessor x = Acc.fromSetGet (put x) (get x)


{-# INLINE lift1 #-}
lift1 ::
   (Enum a, Bits w) =>
   (w -> Int -> w) -> (a -> T w a -> T w a)
lift1 f n (Cons vec) = Cons (f vec (P.fromEnum n))

singleton :: (Enum a, Bits w) => a -> T w a
singleton = P.flip set empty

-- | could also be named @insert@ like in @Set@
set :: (Enum a, Bits w) => a -> T w a -> T w a
set = lift1 B.setBit

-- | could also be named @delete@ like in @Set@
clear :: (Enum a, Bits w) => a -> T w a -> T w a
clear = lift1 B.clearBit

flip :: (Enum a, Bits w) => a -> T w a -> T w a
flip = lift1 B.complementBit

fromBool :: (Enum a, Bits w) => a -> Bool -> T w a
fromBool n b =
   Cons $ if b then B.bit (P.fromEnum n) else U.empty
{- requires Num instance
   Cons (B.shiftL (fromIntegral $ P.fromEnum b) (P.fromEnum n))
-}
