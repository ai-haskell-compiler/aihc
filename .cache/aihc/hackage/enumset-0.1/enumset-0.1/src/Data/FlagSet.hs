{- |
A bit vector that represents a record in a bit-packed way.
-}
module Data.FlagSet (
   T(Cons, decons),
   fromMaskedValue, match,
   Enum(fromEnum),
   compose, decompose,
   Mask(Mask, unmask), maskValue,
   Value(Value, unvalue),
   MaskedValue(MaskedValue),
   get, put, accessor,
   ) where

import Data.Bits (Bits, (.&.), (.|.), )

import Data.Monoid (Monoid(mempty, mappend, mconcat), )
import Data.Semigroup (Semigroup((<>)), )

import qualified Foreign.Storable.Newtype as Store
import Foreign.Storable (Storable(sizeOf, alignment, peek, poke), )

import qualified Data.Accessor.Basic as Acc

import Data.EnumBitSet.Utility (empty, (.-.), )

import qualified Prelude as P
import Prelude hiding (Enum, fromEnum, toEnum, null, flip, )


{- |
The basic bit vector data type.
It does not provide a lot of functionality,
since that could not be done in a safe way.

The type @a@ identifies the maintained flags.
It may be an empty type
but it may also be an enumeration
of record fields with concrete values.
In the latter case you are encouraged to define an 'Enum' instance
for this enumeration.
Be aware that it is different from 'P.Enum' of Prelude.
-}
newtype T word a = Cons {decons :: word}
   deriving (Eq)

instance (Storable w) => Storable (T w a) where
   sizeOf = Store.sizeOf decons
   alignment = Store.alignment decons
   peek = Store.peek Cons
   poke = Store.poke decons


{- |
@Mask w a b@ describes a field of a @T w a@ that has type @Value w b@.
On the machine level a 'Mask' value is a vector of bits,
where set bits represent the bits belonging to one record field.
There must be only one mask value for every pair of types @(a,b)@.
-}
newtype Mask w a b = Mask {unmask :: w}
   deriving (Eq, Show)

{- |
The type parameter @w@ is the type of the underlying bit vector.
The type parameter @b@ is a phantom type,
that is specific for a certain range of bits.
-}
newtype Value w b = Value {unvalue :: w}
   deriving (Eq, Show)


get :: (Enum a, Bits w) => Mask w a b -> T w a -> Value w b
get (Mask m) (Cons fs) = Value (m .&. fs)

{- |
All bits in Value must be contained in the mask.
This condition is not checked by 'put'.

According to names in "Data.Accessor" it should be called @set@,
but in "Data.Bits" and thus "Data.EnumSet"
this is already used in the pair @set@/@clear@.
@put@/@get@ resembles the pair in "Control.Monad.State" in the @mtl@ package.
-}
put :: (Enum a, Bits w) => Mask w a b -> Value w b -> T w a -> T w a
put (Mask m) (Value v) (Cons fs) =
   Cons $ (fs .-. m) .|. v

accessor :: (Enum a, Bits w) => Mask w a b -> Acc.T (T w a) (Value w b)
accessor m = Acc.fromSetGet (put m) (get m)



{- |
Combines a mask with a value, that matches this mask.
In @MaskedValue mask value@, @value@ must be a subset of @mask@.
-}
data MaskedValue w a = MaskedValue w w
   deriving (Eq, Show)


fromMaskedValue :: MaskedValue w a -> T w a
fromMaskedValue (MaskedValue _m v) = Cons v

match :: (Bits w) => T w a -> MaskedValue w a -> Bool
match (Cons fs) (MaskedValue m v) =
   m .&. fs  ==  v


maskValue :: Mask w a b -> Value w b -> MaskedValue w a
maskValue (Mask m) (Value v) = MaskedValue m v


instance (Bits w) => Semigroup (MaskedValue w a) where
   MaskedValue mx vx <> MaskedValue my vy =
      MaskedValue (mx .|. my) (vx .-. my  .|.  vy)

{- |
@mappend a b@ means that values stored in @b@ overwrite corresponding values in @a@.
-}
instance (Bits w) => Monoid (MaskedValue w a) where
   mempty = MaskedValue empty empty
   mappend = (<>)


class Enum a where
   {- |
   'P.fromEnum' should return an integer
   that represents the position of the @a@ value
   in the list of all enumeration items.
   In contrast to that,
   'fromEnum' must return the according bit pattern.
   -}
   fromEnum :: (Bits w) => a -> MaskedValue w a

{- |
Decompose a flag set into flags.
The flags are generated using the 'Bounded' and 'Enum' instance.
We do not recommend to use the result list for further processing,
since testing of flags is much faster using 'match'.
However you may find it useful to 'show' the list.
-}
decompose :: (Bounded a, Enum a, P.Enum a, Bits w) => T w a -> [a]
decompose x =
   filter (match x . fromEnum) [minBound .. maxBound]

{- |
Compose a flag set from a list of flags.
However you may prefer to assemble flags
using 'mconcat' or 'mappend' on 'MaskedValue's.
-}
compose :: (Enum a, P.Enum a, Bits w) => [a] -> T w a
compose xs =
   fromMaskedValue $ mconcat $ map fromEnum xs
