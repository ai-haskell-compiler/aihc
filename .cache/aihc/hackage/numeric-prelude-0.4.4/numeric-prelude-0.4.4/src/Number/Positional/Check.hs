{-# LANGUAGE RebindableSyntax #-}
{- |
Interface to "Number.Positional" which dynamically checks for equal bases.
-}
module Number.Positional.Check where

import qualified Number.Positional as Pos

import qualified Number.Complex as Complex

import qualified Algebra.RealTranscendental as RealTrans
import qualified Algebra.Transcendental     as Trans
import qualified Algebra.Algebraic          as Algebraic
import qualified Algebra.RealField          as RealField
import qualified Algebra.Field              as Field
import qualified Algebra.RealRing           as RealRing
import qualified Algebra.Absolute           as Absolute
import qualified Algebra.Ring               as Ring
import qualified Algebra.Additive           as Additive
import qualified Algebra.ZeroTestable       as ZeroTestable

import qualified Algebra.EqualityDecision as EqDec
import qualified Algebra.OrderDecision    as OrdDec

import qualified Prelude     as P98

import NumericPrelude.Base as P
import NumericPrelude.Numeric as NP


{- |
The value @Cons b e m@
represents the number @b^e * (m!!0 \/ 1 + m!!1 \/ b + m!!2 \/ b^2 + ...)@.
The interpretation of exponent is chosen such that
@floor (logBase b (Cons b e m)) == e@.
That is, it is good for multiplication and logarithms.
(Because of the necessity to normalize the multiplication result,
the alternative interpretation wouldn't be more complicated.)
However for base conversions, roots, conversion to fixed point and
working with the fractional part
the interpretation
@b^e * (m!!0 \/ b + m!!1 \/ b^2 + m!!2 \/ b^3 + ...)@
would fit better.
The digits in the mantissa range from @1-base@ to @base-1@.
The representation is not unique
and cannot be made unique in finite time.
This way we avoid infinite carry ripples.
-}
data T = Cons {base :: Pos.Basis, exponent :: Int, mantissa :: Pos.Mantissa}
   deriving (Show)


{- * basic helpers -}

{- |
Shift digits towards zero by partial application of carries.
E.g. 1.8 is converted to 2.(-2)
If the digits are in the range @(1-base, base-1)@
the resulting digits are in the range @((1-base)/2-2, (base-1)/2+2)@.
The result is still not unique,
but may be useful for further processing.
-}
compress :: T -> T
compress = lift1 Pos.compress


{- | perfect carry resolution, works only on finite numbers -}
carry :: T -> T
carry (Cons b ex xs) =
   let ys = scanr (\x (c,_) -> divMod (x+c) b) (0,undefined) xs
       digits = map snd (init ys)
   in  prependDigit (fst (head ys)) (Cons b ex digits)


prependDigit :: Pos.Digit -> T -> T
prependDigit 0 x = x
prependDigit x (Cons b ex xs) =
   Cons b (ex+1) (x:xs)



{- * conversions -}

lift0 :: (Pos.Basis -> Pos.T) -> T
lift0 op =
   uncurry (Cons defltBase) (op defltBase)

lift1 :: (Pos.Basis -> Pos.T -> Pos.T) -> T -> T
lift1 op (Cons xb xe xm) =
   uncurry (Cons xb) (op xb (xe, xm))

lift2 :: (Pos.Basis -> Pos.T -> Pos.T -> Pos.T) -> T -> T -> T
lift2 op (Cons xb xe xm) (Cons yb ye ym) =
   let b = commonBasis xb yb
   in  uncurry (Cons b) (op b (xe, xm) (ye, ym))

{-
lift4 :: (Int -> Pos.T -> Pos.T -> Pos.T -> Pos.T -> Pos.T) -> T -> T -> T -> T -> T
lift4 op (Cons xb xe xm) (Cons yb ye ym) (Cons zb ze zm) (Cons wb we wm) =
   let b = xb `commonBasis` yb `commonBasis` zb `commonBasis` wb
   in  uncurry (Cons b) (op b (xe, xm) (ye, ym) (ze, zm) (we, wm))
-}

commonBasis :: Pos.Basis -> Pos.Basis -> Pos.Basis
commonBasis xb yb =
   if xb == yb
     then xb
     else error "Number.Positional: bases differ"

fromBaseInteger :: Pos.Basis -> Integer -> T
fromBaseInteger b n =
   uncurry (Cons b) (Pos.fromBaseInteger b n)

fromBaseRational :: Pos.Basis -> Rational -> T
fromBaseRational b r =
   uncurry (Cons b) (Pos.fromBaseRational b r)





defltBaseRoot :: Pos.Basis
defltBaseRoot = 10

defltBaseExp :: Pos.Exponent
defltBaseExp = 3
-- exp 4   let  (sqrt 0.5) fail

defltBase :: Pos.Basis
defltBase = ringPower defltBaseExp defltBaseRoot



defltShow :: T -> String
defltShow (Cons xb xe xm) =
   if xb == defltBase
     then Pos.showBasis defltBaseRoot defltBaseExp (xe,xm)
     else error "defltShow: wrong base"


instance Additive.C T where
   zero   = fromBaseInteger defltBase 0
   (+)    = lift2 Pos.add
   (-)    = lift2 Pos.sub
   negate = lift1 Pos.neg

instance Ring.C T where
   one           = fromBaseInteger defltBase 1
   fromInteger n = fromBaseInteger defltBase n
   (*)           = lift2 Pos.mul

{-
instance Module.C T T where
   (*>) = (*)
-}

instance Field.C T where
   (/)   = lift2 Pos.divide
   recip = lift1 Pos.reciprocal

instance Algebraic.C T where
   sqrt   = lift1 Pos.sqrtNewton
   root n = lift1 (flip Pos.root n)
   x ^/ y = lift1 (flip Pos.power y) x

instance Trans.C T where
   pi     = lift0 Pos.piConst

   exp    = lift1 Pos.exp
   log    = lift1 Pos.ln

   sin    = lift1 (\b -> snd . Pos.cosSin b)
   cos    = lift1 (\b -> fst . Pos.cosSin b)
   tan    = lift1 Pos.tan

   atan   = lift1 Pos.arctan

   {-
   sinh   = lift1 (\b -> snd . Pos.cosSinh b)
   cosh   = lift1 (\b -> snd . Pos.cosSinh b)
   -}

{-
The way EqDec and OrdDec are instantiated
it is possible to have different bases
for the arguments for comparison
and the arguments between we decide.
However, I would not rely on this.
-}
instance EqDec.C T where
   x==?y  =  lift2 (\b -> Pos.ifLazy b (x==y))

instance OrdDec.C T where
   x<=?y  =  lift2 (\b -> Pos.ifLazy b (x<=y))

instance ZeroTestable.C T where
   isZero (Cons xb xe xm) =
      Pos.cmp xb (xe,xm) Pos.zero == EQ

instance Eq T where
   (Cons xb xe xm) == (Cons yb ye ym) =
      Pos.cmp (commonBasis xb yb) (xe,xm) (ye,ym) == EQ

instance Ord T where
   compare (Cons xb xe xm) (Cons yb ye ym) =
      Pos.cmp (commonBasis xb yb) (xe,xm) (ye,ym)

instance Absolute.C T where
   abs = lift1 (const Pos.absolute)
   signum = Absolute.signumOrd

instance RealRing.C T where
   splitFraction (Cons xb xe xm) =
      let (int, frac) = Pos.toFixedPoint xb (xe,xm)
      in  (fromInteger int, Cons xb (-1) frac)

instance RealField.C T where

instance RealTrans.C T where
   atan2  = lift2 (curry . Pos.angle)


-- for complex numbers

instance Complex.Power T where
   power     = Complex.defltPow




-- legacy instances for use of numeric literals in GHCi
instance P98.Num T where
   fromInteger = fromBaseInteger defltBase
   negate = negate -- for unary minus
   (+)    = (+)
   (*)    = (*)
   abs    = abs
   signum = signum

instance P98.Fractional T where
   fromRational = fromBaseRational defltBase . fromRational
   (/) = (/)


{-
MathObj.PowerSeries.approx MathObj.PowerSeries.Example.exp (Number.Positional.fromBaseInteger 10 1) List.!! 30 :: Number.Positional.Check.T
-}
