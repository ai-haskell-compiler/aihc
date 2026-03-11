{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  portable (?)

Quaternions
-}

module Number.Quaternion
        (
        -- * Cartesian form
        T(real,imag),
        fromReal,
        (+::),

        -- * Conversions
        toRotationMatrix,
        fromRotationMatrix,
        fromRotationMatrixDenorm,
        toComplexMatrix,
        fromComplexMatrix,

        -- * Operations
        scalarProduct,
        crossProduct,
        conjugate,
        scale,
        norm,
        normSqr,
        normalize,
        similarity,
        slerp,
        )  where

import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.VectorSpace  as VectorSpace
import qualified Algebra.Module       as Module
import qualified Algebra.Vector       as Vector
import qualified Algebra.Transcendental as Trans
import qualified Algebra.Algebraic    as Algebraic
import qualified Algebra.Field        as Field
import qualified Algebra.Ring         as Ring
import qualified Algebra.Additive     as Additive
import qualified Algebra.ZeroTestable as ZeroTestable

import Algebra.Module((<*>.*>), )

import qualified Number.Complex as Complex

import Number.Complex ((+:))

import qualified NumericPrelude.Elementwise as Elem
import Algebra.Additive ((<*>.+), (<*>.-), (<*>.-$), )

import Data.Array (Array, (!))
import qualified Data.Array as Array

import NumericPrelude.Base
import NumericPrelude.Numeric hiding (signum)
import Text.Show.HT (showsInfixPrec, )
import Text.Read.HT (readsInfixPrec, )


{- TODO:
conversion to and from complex matrix
-}


infix  6  +::, `Cons`

{- |
Quaternions could be defined based on Complex numbers.
However quaternions are often considered as real part and three imaginary parts.
-}
data T a
  = Cons {real :: !a           -- ^ real part
         ,imag :: !(a, a, a)   -- ^ imaginary parts
         }
  deriving (Eq)

fromReal :: Additive.C a => a -> T a
fromReal x = Cons x zero


plusPrec :: Int
plusPrec = 6

instance (Show a) => Show (T a) where
   showsPrec prec (x `Cons` y) = showsInfixPrec "+::" plusPrec prec x y

instance (Read a) => Read (T a) where
   readsPrec prec = readsInfixPrec "+::" plusPrec prec (+::)


-- | Construct a quaternion from real and imaginary part.
(+::) :: a -> (a,a,a) -> T a
(+::) = Cons

-- | The conjugate of a quaternion.
{-# SPECIALISE conjugate :: T Double -> T Double #-}
conjugate        :: (Additive.C a) => T a -> T a
conjugate (Cons r i) =  Cons r (negate i)

-- | Scale a quaternion by a real number.
{-# SPECIALISE scale :: Double -> T Double -> T Double #-}
scale            :: (Ring.C a) => a -> T a -> T a
scale r (Cons xr xi) =  Cons (r * xr) (scaleImag r xi)

-- | like Module.*> but without additional class dependency
scaleImag        :: (Ring.C a) => a -> (a,a,a) -> (a,a,a)
scaleImag r (xi,xj,xk) =  (r * xi, r * xj, r * xk)

-- | the same as NormedEuc.normSqr but with a simpler type class constraint
normSqr          :: (Ring.C a) => T a -> a
normSqr (Cons xr xi) = xr*xr + scalarProduct xi xi

norm             :: (Algebraic.C a) => T a -> a
norm x = sqrt (normSqr x)

-- | scale a quaternion into a unit quaternion
normalize        :: (Algebraic.C a) => T a -> T a
normalize x = scale (recip (norm x)) x

scalarProduct    :: (Ring.C a) => (a,a,a) -> (a,a,a) -> a
scalarProduct (xi,xj,xk) (yi,yj,yk) =
   xi*yi + xj*yj + xk*yk

crossProduct     :: (Ring.C a) => (a,a,a) -> (a,a,a) -> (a,a,a)
crossProduct (xi,xj,xk) (yi,yj,yk) =
   (xj*yk - xk*yj, xk*yi - xi*yk, xi*yj - xj*yi)

{- | similarity mapping as needed for rotating 3D vectors

It holds
@similarity (cos(a\/2) +:: scaleImag (sin(a\/2)) v) (0 +:: x) == (0 +:: y)@
where @y@ results from rotating @x@ around the axis @v@ by the angle @a@.
-}
similarity       :: (Field.C a) => T a -> T a -> T a
similarity c x = c*x/c

{-
rotate   :: (Field.C a) =>
      (a,a,a)  {- ^ rotation axis, must be normalized -}
   -> T a
   -> T a
rotate c x = c*x/c
-}

{- |
Let @c@ be a unit quaternion, then it holds
@similarity c (0+::x) == toRotationMatrix c * x@
-}
toRotationMatrix :: (Ring.C a) => T a -> Array (Int,Int) a
toRotationMatrix (Cons r (i,j,k)) =
   let r2 = r^2
       i2 = i^2;   j2 = j^2;   k2 = k^2
       ri = 2*r*i; rj = 2*r*j; rk = 2*r*k
       jk = 2*j*k; ki = 2*k*i; ij = 2*i*j
   in  Array.listArray ((0,0),(2,2)) $ concat $
          [r2+i2-j2-k2, ij-rk,       ki+rj      ] :
          [ij+rk,       r2-i2+j2-k2, jk-ri      ] :
          [ki-rj,       jk+ri,       r2-i2-j2+k2] :
          []

fromRotationMatrix :: (Algebraic.C a) => Array (Int,Int) a -> T a
fromRotationMatrix =
   normalize . fromRotationMatrixDenorm


checkBounds :: (Int,Int) -> Array (Int,Int) a -> Array (Int,Int) a
checkBounds (c,r) arr =
   let bnds@((c0,r0), (c1,r1)) = Array.bounds arr
   in  if c1-c0==c && r1-r0==r
         then Array.listArray ((0,0), (c1-c0, r1-r0))
                              (Array.elems arr)
         else error ("Quaternion.checkBounds: invalid matrix size "
                         ++ show bnds)


{- |
The rotation matrix must be normalized.
(I.e. no rotation with scaling)
The computed quaternion is not normalized.
-}
fromRotationMatrixDenorm :: (Ring.C a) => Array (Int,Int) a -> T a
fromRotationMatrixDenorm mat' =
   let mat = checkBounds (2,2) mat'
       trace = sum (map (\i -> mat ! (i,i)) [0..2])
       dif (i,j) = mat!(i,j) - mat!(j,i)
   in  Cons (trace+1) (dif (2,1), dif (0,2), dif (1,0))

{- |
Map a quaternion to complex valued 2x2 matrix,
such that quaternion addition and multiplication
is mapped to matrix addition and multiplication.
The determinant of the matrix equals the squared quaternion norm ('normSqr').
Since complex numbers can be turned into real (orthogonal) matrices,
a quaternion could also be converted into a real matrix.
-}
toComplexMatrix :: (Additive.C a) =>
   T a -> Array (Int,Int) (Complex.T a)
toComplexMatrix (Cons r (i,j,k)) =
   Array.listArray ((0,0), (1,1))
      [r+:i, (-j)+:(-k), j+:(-k), r+:(-i)]


{- |
Revert 'toComplexMatrix'.
-}
fromComplexMatrix :: (Field.C a) =>
   Array (Int,Int) (Complex.T a) -> T a
fromComplexMatrix mat =
   let xs = Array.elems (checkBounds (1,1) mat)
       [ar,br,cr,dr] = map Complex.real xs
       [ai,bi,ci,di] = map Complex.imag xs
   in  scale (1/2) (Cons (ar+dr) (ai-di, cr-br, -ci-bi))


{- |
Spherical Linear Interpolation

Can be generalized to any transcendent Hilbert space.
In fact, we should also include the real part in the interpolation.
-}
slerp :: (Trans.C a) =>
      a   {- ^ For @0@ return vector @v@,
               for @1@ return vector @w@ -}
   -> (a,a,a)  {- ^ vector @v@, must be normalized -}
   -> (a,a,a)  {- ^ vector @w@, must be normalized -}
   -> (a,a,a)
slerp c v w =
   let scal  = scalarProduct v w /
                  sqrt (scalarProduct v v * scalarProduct w w)
       angle = Trans.acos scal
   in  scaleImag (recip (Algebraic.sqrt (1-scal^2)))
         (scaleImag (Trans.sin ((1-c)*angle)) v +
          scaleImag (Trans.sin (   c *angle)) w)



instance (NormedEuc.Sqr a b) => NormedEuc.Sqr a (T b) where
   normSqr (Cons r i) = NormedEuc.normSqr r + NormedEuc.normSqr i

instance (Algebraic.C a, NormedEuc.Sqr a b) => NormedEuc.C a (T b) where
   norm = NormedEuc.defltNorm



instance (ZeroTestable.C a) => ZeroTestable.C (T a)  where
   isZero (Cons r i)  = isZero r && isZero i

instance (Additive.C a) => Additive.C (T a)  where
   {-# SPECIALISE instance Additive.C (T Float) #-}
   {-# SPECIALISE instance Additive.C (T Double) #-}
   zero   = Cons zero zero
   (+)    = Elem.run2 $ Elem.with Cons <*>.+  real <*>.+  imag
   (-)    = Elem.run2 $ Elem.with Cons <*>.-  real <*>.-  imag
   negate = Elem.run  $ Elem.with Cons <*>.-$ real <*>.-$ imag

instance (Ring.C a) => Ring.C (T a)  where
   {-# SPECIALISE instance Ring.C (T Float) #-}
   {-# SPECIALISE instance Ring.C (T Double) #-}
   one                          =  Cons one zero
   fromInteger                  =  fromReal . fromInteger
   (Cons xr xi) * (Cons yr yi)  =
       Cons (xr*yr - scalarProduct xi yi)
            (scaleImag xr yi + scaleImag yr xi +
             crossProduct xi yi)

instance (Field.C a) => Field.C (T a)  where
   {-# SPECIALISE instance Field.C (T Float) #-}
   {-# SPECIALISE instance Field.C (T Double) #-}
   recip x = scale (recip (normSqr x)) (conjugate x)
   (Cons xr xi) / y@(Cons yr yi) =
       scale (recip (normSqr y))
          (Cons (xr*yr + scalarProduct xi yi)
                (scaleImag yr xi - scaleImag xr yi - crossProduct xi yi))

instance Vector.C T where
   zero  = zero
   (<+>) = (+)
   (*>)  = scale

-- | The '(*>)' method can't replace 'scale'
--   because it requires the Algebra.Module constraint
instance (Module.C a b) => Module.C a (T b) where
   (*>) = Elem.run2 $ Elem.with Cons <*>.*> real <*>.*> imag

instance (VectorSpace.C a b) => VectorSpace.C a (T b)

