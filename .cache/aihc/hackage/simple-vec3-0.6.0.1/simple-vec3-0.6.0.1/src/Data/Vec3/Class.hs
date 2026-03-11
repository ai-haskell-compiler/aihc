{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vec3.Class
    ( Vec3(..)
    , TVec3
    )

where

import Prelude hiding (zipWith)

import Test.QuickCheck (Arbitrary(..))

-- | Three-dimensional vector, with an associated matrix type.
class Vec3 v where
    -- | Associated type for 3×3 matrix.
    data Matrix v

    -- | Origin point @(0, 0, 0)@.
    origin         :: v
    origin          = fromXYZ (0, 0, 0)
    {-# INLINE origin #-}

    -- | Construct a new vector from components.
    fromXYZ        :: (Double, Double, Double) -> v

    -- | Deconstruct a vector into components.
    toXYZ          :: v -> (Double, Double, Double)

    -- | Zip two vectors elementwise.
    zipWith        :: (Double -> Double -> Double) -> v -> v -> v
    zipWith f v1 v2 = fromXYZ (f x1 x2, f y1 y2, f z1 z2)
                      where
                        (x1, y1, z1) = toXYZ v1
                        (x2, y2, z2) = toXYZ v2
    {-# INLINE zipWith #-}

    -- | Add two vectors.
    (<+>)          :: v -> v -> v
    (<+>)          = zipWith (+)
    {-# INLINE (<+>) #-}
    infixl 7 <+>

    -- | Subtract two vectors.
    (<->)          :: v -> v -> v
    (<->)          = zipWith (-)
    {-# INLINE (<->) #-}
    infixl 7 <->

    -- | Cross product.
    (><)           :: v -> v -> v
    (><) v1 v2      = fromXYZ (y1 * z2 - y2 * z1,
                               x2 * z1 - x1 * z2,
                               x1 * y2 - x2 * y1)
                      where
                        (x1, y1, z1) = toXYZ v1
                        (x2, y2, z2) = toXYZ v2
    infixl 8 ><

    -- | Scale a vector.
    (.^)           :: v -> Double -> v
    (.^) v s        = fromXYZ (x * s, y * s, z * s)
                      where
                        (x, y, z) = toXYZ v
    infixl 9 .^

    -- | Dot product.
    (.*)           :: v -> v -> Double
    (.*) v1 v2      = x + y + z
                      where
                        (x, y, z) = toXYZ $ zipWith (*) v1 v2
    infixl 8 .*

    -- | Euclidean norm of a vector.
    norm           :: v -> Double
    norm v          = sqrt (v .* v)
    {-# INLINE norm #-}

    -- | Produce unit vector with the same direction as the original
    -- one.
    normalize      :: v -> v
    normalize v     = v .^ (1 / norm v)
    {-# INLINE normalize #-}

    -- | Distance between two points.
    distance       :: v -> v -> Double
    distance v1 v2  = norm (v1 <-> v2)
    {-# INLINE distance #-}

    -- | Invert the direction of a vector.
    invert         :: v -> v
    invert v        = origin <-> v
    {-# INLINE invert #-}


    -- | Construct a new matrix from rows.
    fromRows       :: (v, v, v) -> Matrix v

    -- | Deconstruct a matrix into rows.
    toRows         :: Matrix v -> (v, v, v)

    -- | Generic vector dot product.
    --
    -- Multiply the transpose of the first vector by the given matrix,
    -- then multiply the result by the second vector.
    --
    -- @
    --                     [ a11  a12  a13 ]   [ v2x ]
    --                     [               ]   [     ]
    -- [ v1x  v1y  v1z ] . [ a21  a22  a23 ] . [ v2y ] = s
    --                     [               ]   [     ]
    --                     [ a31  a32  a33 ]   [ v2z ]
    -- @
    dotM           :: v -> v -> Matrix v -> Double
    dotM v1 v2 m    = v1 .* (m `mxv` v2)
    {-# INLINE dotM #-}

    -- | Multiply a matrix and a vector.
    --
    -- @
    -- [ a11  a12  a13 ]   [ v2x ]   [ rx ]
    -- [               ]   [     ]   [    ]
    -- [ a21  a22  a23 ] . [ v2y ] = [ ry ]
    -- [               ]   [     ]   [    ]
    -- [ a31  a32  a33 ]   [ v2z ]   [ rz ]
    -- @
    mxv            :: Matrix v -> v -> v
    mxv m v         = fromXYZ (r1 .* v, r2 .* v, r3 .* v)
                      where
                        (r1, r2, r3) = toRows m
    {-# INLINE mxv #-}

    -- | Build a diagonal matrix from a number @d@.
    --
    -- @
    -- [ d  0  0 ]
    -- [         ]
    -- [ 0  d  0 ]
    -- [         ]
    -- [ 0  0  d ]
    -- @
    diag           :: Double -> Matrix v
    diag d          = fromRows
                      (fromXYZ (d, 0, 0),
                       fromXYZ (0, d, 0),
                       fromXYZ (0, 0, d))
    {-# INLINE diag #-}

    -- | Transpose a vector and multiply it by another vector,
    -- producing a matrix.
    --
    -- @
    -- [ v1x ]                       [ r11  r12  r13 ]
    -- [     ]                       [               ]
    -- [ v1y ] . [ v2x  v2y  v2z ] = [ r21  r22  r23 ]
    -- [     ]                       [               ]
    -- [ v1z ]                       [ r31  r32  r33 ]
    -- @
    vxv            :: v -> v -> Matrix v
    vxv v1 v2       = fromRows (v2 .^ v11, v2 .^ v12, v2 .^ v13)
                      where
                        (v11, v12, v13) = toXYZ v1

    {-# INLINE vxv #-}

    -- | Add two matrices.
    addM           :: Matrix v -> Matrix v -> Matrix v
    addM m1 m2      = fromRows (r11 <+> r21,
                                r12 <+> r22,
                                r13 <+> r23)
                      where
                        (r11, r12, r13) = toRows m1
                        (r21, r22, r23) = toRows m2
    {-# INLINE addM #-}


-- | 'Vec3' implementation with 'Data.Vector.Unboxed.Unbox' instance
-- based on default Unbox instance for tuples of arrays, which wraps a
-- vector of tuples as a tuple of vectors.
--
-- @
-- interface:  [v1 (x, y, z); v2 (x, y, z) ...], length = N
--                  |  |  |       |  |  |
-- storage(x): [v1x-+  |  | ; v2x-+  |  |  ...], length = N
-- storage(y): [v1y----+  | ; v2y----+  |  ...], length = N
-- storage(z): [v1z-------+ ; v2z-------+  ...], length = N
-- @
--
-- You almost definitely want to use 'Data.Vec3.CVec3' instead as it has better
-- performance.
type TVec3 = (Double, Double, Double)

instance Vec3 TVec3 where
  newtype Matrix TVec3 = TMatrix { unTMatrix :: (TVec3, TVec3, TVec3) }
                       deriving (Arbitrary, Eq, Show)

  fromXYZ = id
  toXYZ   = id

  fromRows = TMatrix
  toRows   = unTMatrix
