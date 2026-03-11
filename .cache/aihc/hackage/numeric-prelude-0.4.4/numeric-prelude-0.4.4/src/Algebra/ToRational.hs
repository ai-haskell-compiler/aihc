{-# LANGUAGE RebindableSyntax #-}
module Algebra.ToRational where

import qualified Algebra.ZeroTestable as ZeroTestable
import qualified Algebra.Field    as Field
import qualified Algebra.Absolute as Absolute
import Algebra.Field (fromRational, )
import Algebra.Ring (fromInteger, )

import Number.Ratio (Rational, )

import Data.Int  (Int,  Int8,  Int16,  Int32,  Int64,  )
import Data.Word (Word, Word8, Word16, Word32, Word64, )

import qualified Prelude as P
import NumericPrelude.Base
import Prelude (Integer, Float, Double, )

{- |
This class allows lossless conversion
from any representation of a rational to the fixed 'Rational' type.
\"Lossless\" means - don't do any rounding.
For rounding see "Algebra.RealRing".
With the instances for 'Float' and 'Double'
we acknowledge that these types actually represent rationals
rather than (approximated) real numbers.
However, this contradicts to the 'Algebra.Transcendental' class.

Laws that must be satisfied by instances:

>  fromRational' . toRational === id
-}
class (Absolute.C a, ZeroTestable.C a, Ord a) => C a where
   -- | Lossless conversion from any representation of a rational to 'Rational'
   toRational :: a -> Rational

instance C Integer where
   {-# INLINE toRational #-}
   toRational = fromInteger

instance C Float where
   {-# INLINE toRational #-}
   toRational = fromRational . P.toRational

instance C Double where
   {-# INLINE toRational #-}
   toRational = fromRational . P.toRational

instance C Int    where {-# INLINE toRational #-}; toRational = toRational . P.toInteger
instance C Int8   where {-# INLINE toRational #-}; toRational = toRational . P.toInteger
instance C Int16  where {-# INLINE toRational #-}; toRational = toRational . P.toInteger
instance C Int32  where {-# INLINE toRational #-}; toRational = toRational . P.toInteger
instance C Int64  where {-# INLINE toRational #-}; toRational = toRational . P.toInteger

instance C Word   where {-# INLINE toRational #-}; toRational = toRational . P.toInteger
instance C Word8  where {-# INLINE toRational #-}; toRational = toRational . P.toInteger
instance C Word16 where {-# INLINE toRational #-}; toRational = toRational . P.toInteger
instance C Word32 where {-# INLINE toRational #-}; toRational = toRational . P.toInteger
instance C Word64 where {-# INLINE toRational #-}; toRational = toRational . P.toInteger


{- |
It should hold

> realToField = fromRational' . toRational

but it should be much more efficient for particular pairs of types,
such as converting 'Float' to 'Double'.
This achieved by optimizer rules.
-}
{-# NOINLINE [2] realToField #-}
realToField :: (C a, Field.C b) => a -> b
realToField = Field.fromRational' . toRational

{-# RULES
     "NP.realToField :: Integer  -> Float "  realToField = P.realToFrac :: Integer  -> Float ;
     "NP.realToField :: Int      -> Float "  realToField = P.realToFrac :: Int      -> Float ;
     "NP.realToField :: Int8     -> Float "  realToField = P.realToFrac :: Int8     -> Float ;
     "NP.realToField :: Int16    -> Float "  realToField = P.realToFrac :: Int16    -> Float ;
     "NP.realToField :: Int32    -> Float "  realToField = P.realToFrac :: Int32    -> Float ;
     "NP.realToField :: Int64    -> Float "  realToField = P.realToFrac :: Int64    -> Float ;
     "NP.realToField :: Word     -> Float "  realToField = P.realToFrac :: Word     -> Float ;
     "NP.realToField :: Word8    -> Float "  realToField = P.realToFrac :: Word8    -> Float ;
     "NP.realToField :: Word16   -> Float "  realToField = P.realToFrac :: Word16   -> Float ;
     "NP.realToField :: Word32   -> Float "  realToField = P.realToFrac :: Word32   -> Float ;
     "NP.realToField :: Word64   -> Float "  realToField = P.realToFrac :: Word64   -> Float ;
     "NP.realToField :: Float    -> Float "  realToField = P.realToFrac :: Float    -> Float ;
     "NP.realToField :: Double   -> Float "  realToField = P.realToFrac :: Double   -> Float ;
     "NP.realToField :: Integer  -> Double"  realToField = P.realToFrac :: Integer  -> Double;
     "NP.realToField :: Int      -> Double"  realToField = P.realToFrac :: Int      -> Double;
     "NP.realToField :: Int8     -> Double"  realToField = P.realToFrac :: Int8     -> Double;
     "NP.realToField :: Int16    -> Double"  realToField = P.realToFrac :: Int16    -> Double;
     "NP.realToField :: Int32    -> Double"  realToField = P.realToFrac :: Int32    -> Double;
     "NP.realToField :: Int64    -> Double"  realToField = P.realToFrac :: Int64    -> Double;
     "NP.realToField :: Word     -> Double"  realToField = P.realToFrac :: Word     -> Double;
     "NP.realToField :: Word8    -> Double"  realToField = P.realToFrac :: Word8    -> Double;
     "NP.realToField :: Word16   -> Double"  realToField = P.realToFrac :: Word16   -> Double;
     "NP.realToField :: Word32   -> Double"  realToField = P.realToFrac :: Word32   -> Double;
     "NP.realToField :: Word64   -> Double"  realToField = P.realToFrac :: Word64   -> Double;
     "NP.realToField :: Float    -> Double"  realToField = P.realToFrac :: Float    -> Double;
     "NP.realToField :: Double   -> Double"  realToField = P.realToFrac :: Double   -> Double;
  #-}
