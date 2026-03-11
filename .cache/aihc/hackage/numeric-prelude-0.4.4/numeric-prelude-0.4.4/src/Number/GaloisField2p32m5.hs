{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
This number type is intended for tests of functions over fields,
where the field elements need constant space.
This way we can provide a Storable instance.
For 'Rational' this would not be possible.

However, be aware that sums of non-zero elements may yield zero.
Thus division is not always defined, where it is for rational numbers.
-}
module Number.GaloisField2p32m5 where

import qualified Number.ResidueClass as RC
import qualified Algebra.ZeroTestable as ZeroTestable
import qualified Algebra.Module   as Module
import qualified Algebra.Field    as Field
import qualified Algebra.Ring     as Ring
import qualified Algebra.Additive as Additive

import Data.Int (Int64, )
import Data.Word (Word32, Word64, )

import qualified Foreign.Storable.Newtype as SN
import qualified Foreign.Storable as St

import Test.QuickCheck (Arbitrary(arbitrary), )

import NumericPrelude.Base
import NumericPrelude.Numeric


{- $setup
>>> import qualified Number.GaloisField2p32m5 as GF
>>> import qualified Algebra.Laws as Laws
>>> import Test.QuickCheck ((==>))
>>> import NumericPrelude.Numeric
>>> import NumericPrelude.Base
>>> import Prelude ()
>>>
>>> gf :: GF.T -> GF.T
>>> gf = id
-}

{- |
prop> Laws.identity (+) zero . gf
prop> Laws.commutative (+) . gf
prop> Laws.associative (+) . gf
prop> Laws.inverse (+) negate zero . gf
prop> \x -> Laws.inverse (+) (x-) (gf x)
prop> Laws.identity (*) one . gf
prop> Laws.commutative (*) . gf
prop> Laws.associative (*) . gf
prop> \y -> gf y /= zero ==> Laws.inverse (*) recip one y
prop> \y x -> gf y /= zero ==> Laws.inverse (*) (x/) x y
-}
newtype T = Cons {decons :: Word32}
   deriving Eq

{-# INLINE appPrec #-}
appPrec :: Int
appPrec  = 10

instance Show T where
   showsPrec p (Cons x) =
      showsPrec p x
{-
      showParen (p >= appPrec)
         (showString "GF2p32m5.Cons " . shows x)
-}

instance Arbitrary T where
   arbitrary = fmap (Cons . fromInteger . flip mod base) arbitrary

instance St.Storable T where
   sizeOf = SN.sizeOf decons
   alignment = SN.alignment decons
   peek = SN.peek Cons
   poke = SN.poke decons


base :: Ring.C a => a
base = 2^32-5


{-# INLINE lift2 #-}
lift2 :: (Word64 -> Word64 -> Word64) -> (T -> T -> T)
lift2 f (Cons x) (Cons y) =
   Cons (fromIntegral (mod (f (fromIntegral x) (fromIntegral y)) base))

{-# INLINE lift2Integer #-}
lift2Integer :: (Int64 -> Int64 -> Int64) -> (T -> T -> T)
lift2Integer f (Cons x) (Cons y) =
   Cons (fromIntegral (mod (f (fromIntegral x) (fromIntegral y)) base))


instance Additive.C T where
   zero = Cons zero
   (+) = lift2 (+)
--   (-) = lift2 (-)
   x-y = x + negate y
   negate n@(Cons x) =
      if x==0
        then n
        else Cons (base-x)

instance Ring.C T where
   one = Cons one
   (*) = lift2 (*)
   fromInteger =
      Cons . fromInteger . flip mod base

instance Field.C T where
   (/) = lift2Integer (RC.divide base)

instance Module.C T T where
   (*>) = (*)

instance ZeroTestable.C T where
   isZero x  =  zero == x
