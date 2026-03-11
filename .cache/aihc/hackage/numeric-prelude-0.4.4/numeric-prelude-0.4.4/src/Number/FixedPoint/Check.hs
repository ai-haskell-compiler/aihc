{-# LANGUAGE RebindableSyntax #-}
module Number.FixedPoint.Check where

import qualified Number.FixedPoint as FP

import qualified MathObj.PowerSeries.Example as PSE

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Algebraic      as Algebraic
import qualified Algebra.RealRing      as RealRing
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute           as Absolute
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
import qualified Algebra.ZeroTestable   as ZeroTestable

import NumericPrelude.Base
import NumericPrelude.Numeric   hiding (fromRational')

import qualified Prelude        as P98
import qualified NumericPrelude.Numeric as NP


{- * Types -}

data T = Cons {denominator :: Integer, numerator :: Integer}


{- * Conversion -}

cons :: Integer -> Integer -> T
cons = Cons

{- ** other number types -}

fromFloat :: RealRing.C a => Integer -> a -> T
fromFloat den x =
   cons den (FP.fromFloat den x)

fromInteger' :: Integer -> Integer -> T
fromInteger' den x =
   cons den (x * den)

fromRational' :: Integer -> Rational -> T
fromRational' den x =
   cons den (round (x * NP.fromInteger den))

fromFloatBasis :: RealRing.C a => Integer -> Int -> a -> T
fromFloatBasis basis numDigits =
   fromFloat (ringPower numDigits basis)

fromIntegerBasis :: Integer -> Int -> Integer -> T
fromIntegerBasis basis numDigits =
   fromInteger' (ringPower numDigits basis)

fromRationalBasis :: Integer -> Int -> Rational -> T
fromRationalBasis basis numDigits =
   fromRational' (ringPower numDigits basis)

-- | denominator conversion
fromFixedPoint :: Integer -> T -> T
fromFixedPoint denDst (Cons denSrc x) =
   cons denDst (FP.fromFixedPoint denDst denSrc x)


{- * Lift core function -}

lift0 :: Integer -> (Integer -> Integer) -> T
lift0 den f = Cons den (f den)

lift1 :: (Integer -> Integer -> Integer) -> (T -> T)
lift1 f (Cons xd xn) = Cons xd (f xd xn)

lift2 :: (Integer -> Integer -> Integer -> Integer) -> (T -> T -> T)
lift2 f (Cons xd xn) (Cons yd yn) =
   commonDenominator xd yd $ Cons xd (f xd xn yn)

commonDenominator :: Integer -> Integer -> a -> a
commonDenominator xd yd z =
   if xd == yd
     then z
     else error "Number.FixedPoint: denominators differ"


{- * Show -}

appPrec :: Int
appPrec  = 10

instance Show T where
  showsPrec p (Cons den num) =
    showParen (p >= appPrec)
       (showString "FixedPoint.cons " . shows den
          . showString " " . shows num)


defltDenominator :: Integer
defltDenominator = 10^100

defltShow :: T -> String
defltShow (Cons den x) =
   FP.showPositionalDec den x



instance Additive.C T where
   zero   = cons defltDenominator zero
   (+)    = lift2 FP.add
   (-)    = lift2 FP.sub
   negate (Cons xd xn) = Cons xd (negate xn)


instance Ring.C T where
   one         = cons defltDenominator defltDenominator
   fromInteger = fromInteger' defltDenominator . NP.fromInteger
   (*)         = lift2 FP.mul
   -- the default instance of (^) cumulates rounding errors but is faster
   -- x^n           = lift1 (pow n) x


instance Field.C T where
   (/)   = lift2 FP.divide
   recip = lift1 FP.recip
   fromRational' = fromRational' defltDenominator . NP.fromRational'


instance Algebraic.C T where
   sqrt   = lift1 FP.sqrt
   root n = lift1 (FP.root n)


-- these function are only implemented for the convergence radius of their Taylor expansions
instance Trans.C T where
   pi    = lift0 defltDenominator FP.piConst
   exp   = lift1 FP.exp
   log   = lift1 FP.ln
   {-
   logBase
   (**)
   -}
   sin   = lift1 (FP.evalPowerSeries PSE.sin)
   cos   = lift1 (FP.evalPowerSeries PSE.cos)
   -- tan   = lift1 (FP.evalPowerSeries PSE.tan)
   asin  = lift1 (FP.evalPowerSeries PSE.asin)
   atan  = lift1 FP.arctan
   {-
   acos  = lift1 (FP.evalPowerSeries PSE.acos)
   sinh  = lift1 (FP.evalPowerSeries PSE.sinh)
   tanh  = lift1 (FP.evalPowerSeries PSE.tanh)
   cosh  = lift1 (FP.evalPowerSeries PSE.cosh)
   asinh = lift1 (FP.evalPowerSeries PSE.asinh)
   atanh = lift1 (FP.evalPowerSeries PSE.atanh)
   acosh = lift1 (FP.evalPowerSeries PSE.acosh)
   -}


instance ZeroTestable.C T where
   isZero (Cons _ xn)  =  isZero xn

instance Eq T where
   (Cons xd xn) == (Cons yd yn) =
      commonDenominator xd yd (xn==yn)

instance Ord T where
   compare (Cons xd xn) (Cons yd yn) =
      commonDenominator xd yd (compare xn yn)

instance Absolute.C T where
   abs = lift1 (const abs)
   signum = Absolute.signumOrd

instance RealRing.C T where
   splitFraction (Cons xd xn) =
      let (int, frac) = divMod xd xn
      in  (fromInteger int, Cons xd frac)



-- legacy instances for use of numeric literals in GHCi
instance P98.Num T where
   fromInteger = fromInteger' defltDenominator
   negate = negate -- for unary minus
   (+)    = (+)
   (*)    = (*)
   abs    = abs
   signum = signum

instance P98.Fractional T where
   fromRational = fromRational' defltDenominator . fromRational
   (/) = (/)
