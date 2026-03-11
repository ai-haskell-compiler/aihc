{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- Rules should be processed -}
{- |
Module      :  Number.Complex
Copyright   :  (c) The University of Glasgow 2001
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  portable (?)

Complex numbers.
-}

module Number.Complex
        (
        -- * Cartesian form
        T(real,imag),
        imaginaryUnit,
        fromReal,

        (+:),
        (-:),
        scale,
        exp,
        quarterLeft,
        quarterRight,

        -- * Polar form
        fromPolar,
        cis,
        signum,
        signumNorm,
        toPolar,
        magnitude,
        magnitudeSqr,
        phase,
        -- * Conjugate
        conjugate,

        -- * Properties
        propPolar,

        -- * Auxiliary classes
        Power(power),
        defltPow,
        )  where


import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Sum       as NormedSum
import qualified Algebra.NormedSpace.Maximum   as NormedMax

import qualified Algebra.OccasionallyScalar as OccScalar
import qualified Algebra.VectorSpace        as VectorSpace
import qualified Algebra.Module             as Module
import qualified Algebra.Vector             as Vector
import qualified Algebra.RealTranscendental as RealTrans
import qualified Algebra.Transcendental     as Trans
import qualified Algebra.Algebraic          as Algebraic
import qualified Algebra.Field              as Field
import qualified Algebra.Units              as Units
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.IntegralDomain     as Integral
import qualified Algebra.RealRing           as RealRing
import qualified Algebra.Absolute           as Absolute
import qualified Algebra.Ring               as Ring
import qualified Algebra.Additive           as Additive
import qualified Algebra.ZeroTestable       as ZeroTestable
import qualified Algebra.Indexable          as Indexable

import Algebra.Module((<*>.*>), )

import qualified NumericPrelude.Elementwise as Elem
import Algebra.Additive ((<*>.+), (<*>.-), (<*>.-$), )

import Foreign.Storable (Storable (..), )
import qualified Foreign.Storable.Record as Store
import Control.Applicative (liftA2, )

import Test.QuickCheck (Arbitrary, arbitrary, )
import Control.Monad (liftM2, guard, )

import qualified MathObj.Wrapper.Haskell98 as W98

import qualified Prelude as P
import NumericPrelude.Base
import NumericPrelude.Numeric hiding (signum, exp, )
import Text.Show.HT (showsInfixPrec, )
import Text.Read.HT (readsInfixPrec, )



infix  6  +:, `Cons`

{- * The Complex type -}

-- | Complex numbers are an algebraic type.
data T a
  = Cons {real :: !a   -- ^ real part
         ,imag :: !a   -- ^ imaginary part
         }
  deriving (Eq)

{-# INLINE imaginaryUnit #-}
imaginaryUnit :: Ring.C a => T a
imaginaryUnit = zero +: one

{-# INLINE fromReal #-}
fromReal :: Additive.C a => a -> T a
fromReal x = Cons x zero


{-# INLINE plusPrec #-}
plusPrec :: Int
plusPrec = 6

instance (Show a) => Show (T a) where
   showsPrec prec (Cons x y) = showsInfixPrec "+:" plusPrec prec x y

instance (Read a) => Read (T a) where
   readsPrec prec = readsInfixPrec "+:" plusPrec prec (+:)

instance Functor T where
   {-# INLINE fmap #-}
   fmap f (Cons x y) = Cons (f x) (f y)

instance (Arbitrary a) => Arbitrary (T a) where
   {-# INLINE arbitrary #-}
   arbitrary = liftM2 Cons arbitrary arbitrary

instance (Storable a) => Storable (T a) where
   sizeOf    = Store.sizeOf store
   alignment = Store.alignment store
   peek      = Store.peek store
   poke      = Store.poke store

store ::
   (Storable a) =>
   Store.Dictionary (T a)
store =
   Store.run $
   liftA2 (+:)
      (Store.element real)
      (Store.element imag)



{- * Functions -}

-- | Construct a complex number from real and imaginary part.
{-# INLINE (+:) #-}
(+:) :: a -> a -> T a
(+:) = Cons

-- | Construct a complex number with negated imaginary part.
{-# INLINE (-:) #-}
(-:) :: Additive.C a => a -> a -> T a
(-:) x y = Cons x (-y)

-- | The conjugate of a complex number.
{- SPECIALISE conjugate :: T Double -> T Double -}
{-# INLINE conjugate #-}
conjugate :: (Additive.C a) => T a -> T a
conjugate (Cons x y) =  Cons x (-y)

-- | Scale a complex number by a real number.
{- SPECIALISE scale :: Double -> T Double -> T Double -}
{-# INLINE scale #-}
scale :: (Ring.C a) => a -> T a -> T a
scale r =  fmap (r*)

-- | Exponential of a complex number with minimal type class constraints.
{-# INLINE exp #-}
exp :: (Trans.C a) => T a -> T a
exp (Cons x y) =  scale (Trans.exp x) (cis y)

-- | Turn the point one quarter to the right.
{-# INLINE quarterRight #-}
{-# INLINE quarterLeft #-}
quarterRight, quarterLeft :: (Additive.C a) => T a -> T a
quarterRight (Cons x y) = Cons   y  (-x)
quarterLeft  (Cons x y) = Cons (-y)   x

{- | Scale a complex number to magnitude 1.

For a complex number @z@,
@'abs' z@ is a number with the magnitude of @z@,
but oriented in the positive real direction,
whereas @'signum' z@ has the phase of @z@, but unit magnitude.
-}

{- SPECIALISE signum :: T Double -> T Double -}
signum :: (Algebraic.C a, ZeroTestable.C a) => T a -> T a
signum z =
   if isZero z
     then zero
     else scale (recip (magnitude z)) z

{- SPECIALISE signumNorm :: T Double -> T Double -}
{-# INLINE signumNorm #-}
signumNorm :: (Algebraic.C a, NormedEuc.C a a, ZeroTestable.C a) => T a -> T a
signumNorm z =
   if isZero z
     then zero
     else scale (recip (NormedEuc.norm z)) z

-- | Form a complex number from polar components of magnitude and phase.
{- SPECIALISE fromPolar :: Double -> Double -> T Double -}
{-# INLINE fromPolar #-}
fromPolar :: (Trans.C a) => a -> a -> T a
fromPolar r theta =  scale r (cis theta)

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{- SPECIALISE cis :: Double -> T Double -}
{-# INLINE cis #-}
cis :: (Trans.C a) => a -> T a
cis theta =  Cons (cos theta) (sin theta)

propPolar :: (RealTrans.C a, ZeroTestable.C a) => T a -> Bool
propPolar z =  uncurry fromPolar (toPolar z) == z


{- |
The nonnegative magnitude of a complex number.
This implementation respects the limited range of floating point numbers.
The trivial implementation 'magnitude'
would overflow for floating point exponents greater than
the half of the maximum admissible exponent.
We automatically drop in this implementation for 'Float' and 'Double'
by optimizer rules.
You should do so for your custom floating point types.
-}
{-# INLINE floatMagnitude #-}
floatMagnitude :: (P.RealFloat a, Algebraic.C a) => T a -> a
floatMagnitude (Cons x y) =
   let k  = max (P.exponent x) (P.exponent y)
       mk = - k
   in  P.scaleFloat k
           (sqrt (P.scaleFloat mk x ^ 2 +
                  P.scaleFloat mk y ^ 2))

{-# INLINE [1] magnitude #-}
magnitude :: (Algebraic.C a) => T a -> a
magnitude = sqrt . magnitudeSqr

{-# RULES
     "Complex.magnitude :: Double"
        magnitude = floatMagnitude :: T Double -> Double;

     "Complex.magnitude :: Float"
        magnitude = floatMagnitude :: T Float -> Float;
  #-}

-- like NormedEuc.normSqr with lifted class constraints
{-# INLINE magnitudeSqr #-}
magnitudeSqr :: (Ring.C a) => T a -> a
magnitudeSqr (Cons x y) = x^2 + y^2

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
{-# INLINE phase #-}
phase :: (RealTrans.C a, ZeroTestable.C a) => T a -> a
phase z =
   if isZero z
     then zero   -- SLPJ July 97 from John Peterson
     else case z of (Cons x y) -> atan2 y x


{- |
The function 'toPolar' takes a complex number and
returns a (magnitude, phase) pair in canonical form:
the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
if the magnitude is zero, then so is the phase.
-}
toPolar :: (RealTrans.C a, ZeroTestable.C a) => T a -> (a,a)
toPolar z = (magnitude z, phase z)



{- * Instances of T -}

{-
complexTc = Ty.mkTyCon "Complex.T"
instance Ty.Typeable1 T where { typeOf1 _ = Ty.mkTyConApp complexTc [] }
instance Ty.Typeable a => Ty.Typeable (T a) where { typeOf = Ty.typeOfDefault }
-}

instance  (Indexable.C a) => Indexable.C (T a) where
    {-# INLINE compare #-}
    compare (Cons x y) (Cons x' y')  =  Indexable.compare (x,y) (x',y')

instance  (ZeroTestable.C a) => ZeroTestable.C (T a)  where
    {-# INLINE isZero #-}
    isZero (Cons x y)  = isZero x && isZero y

instance  (Additive.C a) => Additive.C (T a)  where
    {- SPECIALISE instance Additive.C (T Float) -}
    {- SPECIALISE instance Additive.C (T Double) -}
    {-# INLINE zero #-}
    {-# INLINE negate #-}
    {-# INLINE (+) #-}
    {-# INLINE (-) #-}
    zero   = Cons zero zero
    (+)    = Elem.run2 $ Elem.with Cons <*>.+  real <*>.+  imag
    (-)    = Elem.run2 $ Elem.with Cons <*>.-  real <*>.-  imag
    negate = Elem.run  $ Elem.with Cons <*>.-$ real <*>.-$ imag

instance  (Ring.C a) => Ring.C (T a)  where
    {- SPECIALISE instance Ring.C (T Float) -}
    {- SPECIALISE instance Ring.C (T Double) -}
    {-# INLINE one #-}
    one                         =  Cons one zero
    {-# INLINE (*) #-}
    (Cons x y) * (Cons x' y')   =  Cons (x*x'-y*y') (x*y'+y*x')
    {-# INLINE fromInteger #-}
    fromInteger                 =  fromReal . fromInteger

instance  (Absolute.C a, Algebraic.C a, ZeroTestable.C a) => Absolute.C (T a)  where
    {- SPECIALISE instance Absolute.C (T Float) -}
    {- SPECIALISE instance Absolute.C (T Double) -}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    abs x  = Cons (magnitude x) zero
    signum = signum

instance Vector.C T where
   {-# INLINE zero #-}
   zero  = zero
   {-# INLINE (<+>) #-}
   (<+>) = (+)
   {-# INLINE (*>) #-}
   (*>)  = scale

-- | The '(*>)' method can't replace 'scale'
--   because it requires the Algebra.Module constraint
instance (Module.C a b) => Module.C a (T b) where
   {-# INLINE (*>) #-}
   (*>) = Elem.run2 $ Elem.with Cons <*>.*> real <*>.*> imag
   -- s *> (Cons x y)  = Cons (s *> x) (s *> y)

instance (VectorSpace.C a b) => VectorSpace.C a (T b)

instance (Additive.C a, NormedSum.C a v) => NormedSum.C a (T v) where
   {-# INLINE norm #-}
   norm x = NormedSum.norm (real x) + NormedSum.norm (imag x)

instance (NormedEuc.Sqr a b) => NormedEuc.Sqr a (T b) where
   {-# INLINE normSqr #-}
   normSqr x = NormedEuc.normSqr (real x) + NormedEuc.normSqr (imag x)

instance (Algebraic.C a, NormedEuc.Sqr a b) => NormedEuc.C a (T b) where
   {-# INLINE norm #-}
   norm = NormedEuc.defltNorm

instance (Ord a, NormedMax.C a v) => NormedMax.C a (T v) where
   {-# INLINE norm #-}
   norm x = max (NormedMax.norm (real x)) (NormedMax.norm (imag x))

instance (Show v, ZeroTestable.C v, Additive.C v, OccScalar.C a v) => OccScalar.C a (T v) where
   toScalar        = OccScalar.toScalarShow
   toMaybeScalar x =
      guard (isZero (imag x)) >>
      OccScalar.toMaybeScalar (real x)
   fromScalar      = fromReal . OccScalar.fromScalar


{-
  In this implementation the complex plane is structured
  as an orthogonal grid induced by the divisor z'.
  The coordinate of a cell within this grid is returned as quotient
  and the position of the cell in the grid is returned as remainder.
  The magnitude of the remainder might be larger than that of the divisor
  thus the Euclidean algorithm can fail.
-}

instance  (Integral.C a) => Integral.C (T a)  where
    divMod z z' =
       let denom = magnitudeSqr z'
           zBig  = z * conjugate z'
           q     = fmap (flip div denom) zBig
       in  (q, z-q*z')


{-
  This variant of divMod tries to come close to the origin.
  Thus the remainder has smaller magnitude than the divisor.
  This variant of divModCent can be used for Euclidean's algorithm.
-}
{-# INLINE divModCent #-}
divModCent :: (Ord a, Integral.C a) => T a -> T a -> (T a, T a)
divModCent z z' =
   let denom = magnitudeSqr z'
       zBig  = z * conjugate z'
       re    = divMod (real zBig) denom
       im    = divMod (imag zBig) denom
       q  = Cons (fst re) (fst im)
       r  = Cons (snd re) (snd im)
       q' = Cons
              (real q + if 2 * real r > denom then one else zero)
              (imag q + if 2 * imag r > denom then one else zero)
   in  (q', z-q'*z')

{-# INLINE modCent #-}
modCent :: (Ord a, Integral.C a) => T a -> T a -> T a
modCent z z' = snd (divModCent z z')

instance  (Ord a, Units.C a) => Units.C (T a)  where
    {-# INLINE isUnit #-}
    isUnit (Cons x y) =
       isUnit x && y==zero  ||
       isUnit y && x==zero
    {-# INLINE stdAssociate #-}
    stdAssociate z@(Cons x y) =
       let z' = if y<0  ||  y==0 && x<0 then negate z else z
       in  if real z'<=0 then quarterRight z' else z'
    {-# INLINE stdUnit #-}
    stdUnit z@(Cons x y) =
       if z==zero
         then 1
         else
           let (x',sgn') = if y<0  ||  y==0 && x<0
                             then (negate x, -1)
                             else (x, 1)
           in  if x'<=0 then quarterLeft sgn' else sgn'


instance  (Ord a, ZeroTestable.C a, Units.C a) => PID.C (T a) where
   {-# INLINE gcd #-}
   gcd         = euclid modCent
   {-# INLINE extendedGCD #-}
   extendedGCD = extendedEuclid divModCent


{-# INLINE [1] divide #-}
divide :: (Field.C a) => T a -> T a -> T a
divide (Cons x y) z'@(Cons x' y') =
   let d = magnitudeSqr z'
   in  Cons ((x*x'+y*y') / d) ((y*x'-x*y') / d)

-- | Special implementation of @(\/)@ for floating point numbers
--   which prevent intermediate overflows.
{-# INLINE floatDivide #-}
floatDivide :: (P.RealFloat a, Field.C a) => T a -> T a -> T a
floatDivide (Cons x y) (Cons x' y') =
   let k   = - max (P.exponent x') (P.exponent y')
       x'' = P.scaleFloat k x'
       y'' = P.scaleFloat k y'
       d   = x'*x'' + y'*y''
   in  Cons ((x*x''+y*y'') / d) ((y*x''-x*y'') / d)

{-# RULES
     "Complex.divide :: Double"
        divide = floatDivide :: T Double -> T Double -> T Double;

     "Complex.divide :: Float"
        divide = floatDivide :: T Float -> T Float -> T Float;
  #-}




instance  (Field.C a) => Field.C (T a)  where
    {-# INLINE (/) #-}
    (/)                 =  divide
    {-# INLINE fromRational' #-}
    fromRational'       =  fromReal . fromRational'

{-|
   We like to build the Complex Algebraic instance
   on top of the Algebraic instance of the scalar type.
   This poses no problem to 'sqrt'.
   However, 'Number.Complex.root' requires computing the complex argument
   which is a transcendent operation.
   In order to keep the type class dependencies clean
   for more sophisticated algebraic number types,
   we introduce a type class which actually performs the radix operation.
-}
class (Algebraic.C a) => (Power a) where
    power  ::  Rational -> T a -> T a


{-# INLINE defltPow #-}
defltPow :: (RealTrans.C a, ZeroTestable.C a) =>
    Rational -> T a -> T a
defltPow r x =
    let (mag,arg) = toPolar x
    in  fromPolar (mag ^/ r)
                  (arg * fromRational' r)


instance  Power Float where
    {-# INLINE power #-}
    power  =  defltPow

instance  Power Double where
    {-# INLINE power #-}
    power  =  defltPow


instance  (RealRing.C a, Algebraic.C a, Power a) =>
          Algebraic.C (T a)  where
    -- | the real part of the result is always non-negative
    {-# INLINE sqrt #-}
    sqrt z@(Cons x y)  =  if z == zero
                            then zero
                            else
                              let u'    = sqrt ((magnitude z + abs x) / 2)
                                  v'    = abs y / (u'*2)
                                  (u,v) = if x < 0 then (v',u') else (u',v')
                              in  Cons u (if y < 0 then -v else v)
    {-# INLINE (^/) #-}
    (^/) = flip power


instance  (RealRing.C a, RealTrans.C a, ZeroTestable.C a, Power a) =>
          Trans.C (T a)  where
    {- SPECIALISE instance Trans.C (T Float) -}
    {- SPECIALISE instance Trans.C (T Double) -}
    {-# INLINE pi #-}
    pi                 =  fromReal pi
    {-# INLINE exp #-}
    exp                =  exp
    {-# INLINE log #-}
    log z              =  let (m,p) = toPolar z in Cons (log m) p

    -- use defaults for tan, tanh

    {-# INLINE sin #-}
    sin (Cons x y)     =  Cons (sin x * cosh y) (  cos x * sinh y)
    {-# INLINE cos #-}
    cos (Cons x y)     =  Cons (cos x * cosh y) (- sin x * sinh y)

    {-# INLINE sinh #-}
    sinh (Cons x y)    =  Cons (cos y * sinh x) (sin y * cosh x)
    {-# INLINE cosh #-}
    cosh (Cons x y)    =  Cons (cos y * cosh x) (sin y * sinh x)

    {-# INLINE asin #-}
    asin z             =  quarterRight (log (quarterLeft z + sqrt (1 - z^2)))
    {-# INLINE acos #-}
    acos z             =  quarterRight (log (z + quarterLeft (sqrt (1 - z^2))))
    {-# INLINE atan #-}
    atan z@(Cons x y)  =  quarterRight (log (Cons (1-y) x / sqrt (1+z^2)))

{- use the default implementation
    asinh z        =  log (z + sqrt (1+z^2))
    acosh z        =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
    atanh z        =  log ((1+z) / sqrt (1-z^2))
-}


-- * Haskell 98 legacy instances

-- legacy instances for use of numeric literals in GHCi
instance (P.Floating a, Eq a) => P.Num (T a) where
   {-# INLINE fromInteger #-}
   fromInteger n = Cons (P.fromInteger n) (P.fromInteger 0)
   {-# INLINE negate #-}
   negate = W98.unliftF1 Additive.negate
   {-# INLINE (+) #-}
   (+)    = W98.unliftF2 (Additive.+)
   {-# INLINE (*) #-}
   (*)    = W98.unliftF2 (Ring.*)
   {-# INLINE abs #-}
   abs    = W98.unliftF1 Absolute.abs
   {-# INLINE signum #-}
   signum = W98.unliftF1 Absolute.signum

instance (P.Floating a, Eq a) => P.Fractional (T a) where
   {-# INLINE fromRational #-}
   fromRational x = Cons (P.fromRational x) (P.fromInteger 0)
   {-# INLINE (/) #-}
   (/) = W98.unliftF2 (Field./)
