{-|
Module      : Math.ExpPairs.Matrix3
Copyright   : (c) Andrew Lelechenko, 2014-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Matrices of order 3
and efficient multiplication algorithms.

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Safe              #-}

module Math.ExpPairs.Matrix3
  ( Matrix3 (..)
  , fromList
  , det
  , multCol
  , normalize
  , makarovMult
  , ladermanMult
  ) where

import Prelude hiding (foldl1)

import Control.DeepSeq
import Data.Foldable  (Foldable (..), toList)
import Data.List      (transpose)
import GHC.Generics   (Generic (..))
import Prettyprinter

-- | Matrix of order 3.
data Matrix3 t = Matrix3 {
  a11 :: !t,
  a12 :: !t,
  a13 :: !t,
  a21 :: !t,
  a22 :: !t,
  a23 :: !t,
  a31 :: !t,
  a32 :: !t,
  a33 :: !t
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData t => NFData (Matrix3 t) where
  rnf = rnf . toList

diag :: Num t => t -> Matrix3 t
diag n = Matrix3 {
  a11 = n,
  a12 = 0,
  a13 = 0,
  a21 = 0,
  a22 = n,
  a23 = 0,
  a31 = 0,
  a32 = 0,
  a33 = n
  }

-- | 'fromInteger' returns a diagonal matrix
instance Num t => Num (Matrix3 t) where
  a + b = Matrix3 {
    a11 = a11 a + a11 b,
    a12 = a12 a + a12 b,
    a13 = a13 a + a13 b,
    a21 = a21 a + a21 b,
    a22 = a22 a + a22 b,
    a23 = a23 a + a23 b,
    a31 = a31 a + a31 b,
    a32 = a32 a + a32 b,
    a33 = a33 a + a33 b
    }

  (*)         = usualMult
  negate      = fmap negate
  abs         = id
  signum      = id
  fromInteger = diag . fromInteger

usualMult :: Num t => Matrix3 t -> Matrix3 t -> Matrix3 t
usualMult a b = Matrix3 {
    a11 = a11 a * a11 b + a12 a * a21 b + a13 a * a31 b,
    a12 = a11 a * a12 b + a12 a * a22 b + a13 a * a32 b,
    a13 = a11 a * a13 b + a12 a * a23 b + a13 a * a33 b,
    a21 = a21 a * a11 b + a22 a * a21 b + a23 a * a31 b,
    a22 = a21 a * a12 b + a22 a * a22 b + a23 a * a32 b,
    a23 = a21 a * a13 b + a22 a * a23 b + a23 a * a33 b,
    a31 = a31 a * a11 b + a32 a * a21 b + a33 a * a31 b,
    a32 = a31 a * a12 b + a32 a * a22 b + a33 a * a32 b,
    a33 = a31 a * a13 b + a32 a * a23 b + a33 a * a33 b
    }
{-# SPECIALIZE usualMult :: Matrix3 Int -> Matrix3 Int -> Matrix3 Int #-}
{-# SPECIALIZE usualMult :: Matrix3 Integer -> Matrix3 Integer -> Matrix3 Integer #-}

-- | Multiplicate matrices. Requires 23 multiplications and 62 additions.
-- It becomes faster than vanilla multiplication '(*)',
-- which requires 27 multiplications and 18 additions,
-- when matrix's elements are large (> 700 digits) integers.
--
-- An algorithm follows
-- /J. Laderman./
-- A noncommutative algorithm for multiplying \( 3 \times 3 \)
-- matrices using 23 multiplications.
-- Bull. Amer. Math. Soc., 82:126–128, 1976.
-- Our contribution is reducing the number of additions
-- from 98 to 62 by well-thought choice of intermediate variables.
ladermanMult :: Num t => Matrix3 t -> Matrix3 t -> Matrix3 t
ladermanMult
  (Matrix3 a11 a12 a13 a21 a22 a23 a31 a32 a33)
  (Matrix3 b11 b12 b13 b21 b22 b23 b31 b32 b33)
  = Matrix3 c11 c12 c13 c21 c22 c23 c31 c32 c33 where
    t33 = t37 + a22
    t34 = t38 + a32
    t35 = t39 - a32
    t36 = a22 - t40
    t37 = a23 - a13
    t38 = a31 - a11
    t39 = a13 - a33
    t40 = a11 - a21

    u33 = b23 - u37
    u34 = u38 - b22
    u35 = b11 - u39
    u36 = b22 - u40
    u37 = b33 - b31
    u38 = b32 - b31
    u39 = b13 - b23
    u40 = b12 - b11

    m1 = (t35 + a12 - t36) * b22
    m2 = t40 * (b22 - b12)
    m3 = a22 * (b21 - u36 - u33)
    m4 = t36 * u36
    m5 = (a22 + a21) * u40
    m6 = a11 * b11
    m7 = t34 * u35
    m8 = t38 * u39
    m9 = (a31 + a32) * (b13 - b11)
    m10 = (a12 - t33 - t34) * b23
    m11 = a32 * (u34 + b21 - u35)
    m12 = t35 * u34
    m13 = t39 * (b22 - b32)
    m14 = a13 * b31
    m15 = (a33 + a32) * u38
    m16 = t33 * u33
    m17 = t37 * (b33 - b23)
    m18 = (a23 + a22) * u37
    m19 = a12 * b21
    m20 = a23 * b32
    m21 = a21 * b13
    m22 = a31 * b12
    m23 = a33 * b33

    v33 = m12 + m14
    v34 = m16 + m14
    v35 = m4 + m6
    v36 = m7 + m6
    v37 = v33 + m15
    v38 = v35 + m5
    v39 = v34 + m18
    v40 = v36 + m9

    c11 = m6 + m19 + m14
    c12 = v38 + v37 + m1
    c13 = v40 + v39 + m10
    c21 = v35 + v34 + m3 + m2 + m17
    c22 = v38 + m20 + m2
    c23 = v39 + m21 + m17
    c31 = v36 + v33 + m8 + m13 + m11
    c32 = v37 + m22 + m13
    c33 = v40 + m23 + m8
{-# SPECIALIZE ladermanMult :: Matrix3 Integer -> Matrix3 Integer -> Matrix3 Integer #-}

-- | Multiplicate matrices under assumption that multiplication of elements is commutative.
-- Requires 22 multiplications and 66 additions.
-- It becomes faster than vanilla multiplication '(*)',
-- which requires 27 multiplications and 18 additions,
-- when matrix's elements are large (> 700 digits) integers.
--
-- An algorithm follows
-- /O. M. Makarov./
-- An algorithm for multiplication of \( 3 \times 3 \) matrices.
-- Zh. Vychisl. Mat. i Mat. Fiz., 26(2):293–294, 320, 1986.
-- Our contribution is reducing the number of additions
-- from 105 to 66 by well-thought choice of intermediate variables.
makarovMult :: Num t => Matrix3 t -> Matrix3 t -> Matrix3 t
makarovMult
  (Matrix3 k1 b1 c1 k2 b2 c2 k3 b3 c3)
  (Matrix3 a1 a2 a3 k4 k5 k6 k7 k8 k9)
  = Matrix3 c11 c12 c13 c21 c22 c23 c31 c32 c33 where
    t32 = c3 + c2
    t33 = b3 + b1
    t34 = c1 - c2
    t35 = b2 + b1

    u32 = k4 + k6 - k5
    u33 = k9 + k7 - k8
    u34 = k6 + k8

    m1 = (t34 + a3) * (u33 + k1)
    m2 = (t35 + a2) * (k2 - u32)
    m3 = (t33 + a2) * (k3 - u32)
    m4 = (a3 - t32) * (k3 - u33)
    m5 = (a1 - t34) * k1
    m6 = (t35 + a1) * k2
    m7 = (t33 + t32 + a1) * k3
    m8 = a2 * (k1 + u32)
    m9 = a3 * (u33 + k2)
    m10 = b1 * k4
    m11 = c2 * k7
    m12 = t34 * (k7 + k1)
    m13 = t35 * (k4 - k2)
    m14 = (b1 + a2) * u32
    m15 = b2 * k6
    m16 = (a3 - c2) * u33
    m17 = c2 * k8
    m18 = (b3 - t32) * k6
    m19 = (c3 + c1 - t33) * k8
    m20 = t33 * (u34 + k4 - k3)
    m21 = t32 * (u34 + k3 - k7)
    m22 = (t32 - t33) * u34

    v32 = v38 - v35
    v33 = v35 - v36
    v34 = m19 - m22
    v35 = m17 - m18
    v36 = m14 - m10
    v37 = m11 + m10
    v38 = m16 + m11
    v39 = m20 + m22
    v40 = m15 + m17

    c11 = v37 + m5 + m12
    c12 = v34 + v33 + m8
    c13 = v34 + m1 - m12 - v32
    c21 = m6 + m13 + m11 - m10
    c22 = v40 + v36 + m2 + m13
    c23 = v40 + m9 - v38
    c31 = v39 + m7 - m21 - v37
    c32 = v39 + m3 - v33
    c33 = v32 + m4 + m21
{-# SPECIALIZE makarovMult :: Matrix3 Integer -> Matrix3 Integer -> Matrix3 Integer #-}

-- | Compute the determinant of a matrix.
det :: Num t => Matrix3 t -> t
det Matrix3 {..} =
  a11 * (a22 * a33 - a32 * a23)
  - a12 * (a21 * a33 - a23 * a31)
  + a13 * (a21 * a32 - a22 * a31)

-- | 'fromRational' returns a diagonal matrix
instance Fractional t => Fractional (Matrix3 t) where
  fromRational = diag . fromRational

  recip a@Matrix3{..} = Matrix3 {
    a11 =  (a22 * a33 - a32 * a23) / d,
    a12 = -(a21 * a33 - a23 * a31) / d,
    a13 =  (a21 * a32 - a22 * a31) / d,
    a21 = -(a12 * a33 - a13 * a32) / d,
    a22 =  (a11 * a33 - a13 * a31) / d,
    a23 = -(a11 * a32 - a12 * a31) / d,
    a31 =  (a12 * a23 - a13 * a22) / d,
    a32 = -(a11 * a23 - a13 * a21) / d,
    a33 =  (a11 * a22 - a12 * a21) / d
    } where d = det a

-- | Convert a list of 9 elements into 'Matrix3'.
-- Reverse conversion can be done by 'Data.Foldable.toList'.
fromList :: [t] -> Matrix3 t
fromList [a11, a12, a13, a21, a22, a23, a31, a32, a33] = Matrix3 {
  a11 = a11,
  a12 = a12,
  a13 = a13,
  a21 = a21,
  a22 = a22,
  a23 = a23,
  a31 = a31,
  a32 = a32,
  a33 = a33
  }
fromList _ = error "fromList: input must contain exactly 9 elements"

-- | Divide all elements of the matrix by their greatest common
-- divisor. This is useful for matrices of projective
-- transformations to reduce the magnitude of computations.
normalize :: Integral t => Matrix3 t -> Matrix3 t
normalize a@Matrix3{..}
  | d <= 1    = a
  | otherwise = fmap (`quot` d) a
  where
    d = go a11 [a12, a13, a21, a22, a23, a31, a32, a33]
    go 1 _ = 1
    go 2 xs = if all even xs then 2 else 1
    go acc [] = acc
    go acc (x : xs) = go (gcd acc x) xs
{-# SPECIALIZE normalize :: Matrix3 Integer -> Matrix3 Integer #-}

instance Pretty t => Pretty (Matrix3 t) where
  pretty = vsep . map hsep . pad . fmap pretty where
    pad Matrix3{..} = map (zipWith fill ls) table where
      table = [[a11, a12, a13], [a21, a22, a23], [a31, a32, a33]]
      ls = map (maximum . map (length . show)) (transpose table)

-- | Multiplicate a matrix by a column vector.
multCol :: Num t => Matrix3 t -> (t, t, t) -> (t, t, t)
multCol Matrix3 {..} (a1, a2, a3) = (
  a11 * a1 + a12 * a2 + a13 * a3,
  a21 * a1 + a22 * a2 + a23 * a3,
  a31 * a1 + a32 * a2 + a33 * a3
  )
{-# INLINE multCol #-}
