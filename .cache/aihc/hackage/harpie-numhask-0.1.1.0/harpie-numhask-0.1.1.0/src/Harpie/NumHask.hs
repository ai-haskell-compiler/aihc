{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | numhask orphan instances and shim for harpie.
module Harpie.NumHask
  ( -- * Usage
    -- $usage
    ident,
    undiag,
    mult,
    invtri,
    inverse,
    chol,
  )
where

import Data.Functor.Rep
import Fcf qualified
import GHC.TypeNats
import Harpie.Fixed as F hiding (chol, ident, inverse, invtri, mult, undiag)
import Harpie.Shape (DeleteDims, Eval, GetDims, KnownNats, Rank, type (++))
import Harpie.Shape qualified as S
import NumHask.Prelude as P hiding (Min, cycle, diff, drop, empty, find, length, repeat, sequence, take, zipWith)

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude hiding (cycle, repeat, empty, diff, take, drop, zipWith, find)
-- >>> import Harpie.Fixed qualified as F
-- >>> import Harpie.Fixed (Array, array, range, shape, toDynamic)
-- >>> import Harpie.NumHask
-- >>> import Prettyprinter hiding (dot,fill)
--
-- >>> s = 1 :: Array '[] Int
-- >>> s
-- [1]
-- >>> shape s
-- []
-- >>> pretty s
-- 1
-- >>> let v = range @'[3]
-- >>> pretty v
-- [0,1,2]
-- >>> let m = range @[2,3]
-- >>> pretty m
-- [[0,1,2],
--  [3,4,5]]
-- >>> a = range @[2,3,4]
-- >>> a
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]
-- >>> toDynamic a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]

-- * NumHask heirarchy

instance
  ( Additive a,
    KnownNats s
  ) =>
  Additive (Array s a)
  where
  (+) = liftR2 (+)

  zero = pureRep zero

instance
  ( Subtractive a,
    KnownNats s
  ) =>
  Subtractive (Array s a)
  where
  negate = fmapRep negate

instance
  (Multiplicative a) =>
  MultiplicativeAction (Array s a)
  where
  type Scalar (Array s a) = a
  (|*) r s = fmap (s *) r

instance (Additive a) => AdditiveAction (Array s a) where
  type AdditiveScalar (Array s a) = a
  (|+) r s = fmap (s +) r

instance
  (Subtractive a) =>
  SubtractiveAction (Array s a)
  where
  (|-) r s = fmap (\x -> x - s) r

instance
  (Divisive a) =>
  DivisiveAction (Array s a)
  where
  (|/) r s = fmap (/ s) r

instance (KnownNats s, JoinSemiLattice a) => JoinSemiLattice (Array s a) where
  (\/) = liftR2 (\/)

instance (KnownNats s, MeetSemiLattice a) => MeetSemiLattice (Array s a) where
  (/\) = liftR2 (/\)

instance (KnownNats s, Subtractive a, Epsilon a) => Epsilon (Array s a) where
  epsilon = konst epsilon

instance (FromInteger a) => FromInteger (Array ('[] :: [Nat]) a) where
  fromInteger x = toScalar (fromInteger x)

instance (FromRational a) => FromRational (Array ('[] :: [Nat]) a) where
  fromRational x = toScalar (fromRational x)

-- | The identity array.
--
-- >>> pretty $ ident @[3,3]
-- [[1,0,0],
--  [0,1,0],
--  [0,0,1]]
ident :: (KnownNats s, Additive a, Multiplicative a) => Array s a
ident = tabulate (bool zero one . S.isDiag . S.fromFins)

-- | Expand the array to form a diagonal array
--
-- >>> pretty $ undiag (range @'[3])
-- [[0,0,0],
--  [0,1,0],
--  [0,0,2]]
undiag ::
  forall s' a s.
  ( KnownNats s,
    KnownNats s',
    s' ~ Eval ((++) s s),
    Additive a
  ) =>
  Array s a ->
  Array s' a
undiag a = tabulate (\xs -> bool zero (index a (S.UnsafeFins $ pure $ S.getDim 0 (S.fromFins xs))) (S.isDiag (S.fromFins xs)))

-- | Array multiplication.
--
-- matrix multiplication
--
-- >>> pretty $ mult m (F.transpose m)
-- [[5,14],
--  [14,50]]
--
-- inner product
--
-- >>> pretty $ mult v v
-- 5
--
-- matrix-vector multiplication
--
-- >>> pretty $ mult v (F.transpose m)
-- [5,14]
--
-- >>> pretty $ mult m v
-- [5,14]
mult ::
  forall a ds0 ds1 s0 s1 so0 so1 st si.
  ( Ring a,
    KnownNats s0,
    KnownNats s1,
    KnownNats ds0,
    KnownNats ds1,
    KnownNats so0,
    KnownNats so1,
    KnownNats st,
    KnownNats si,
    so0 ~ Eval (DeleteDims ds0 s0),
    so1 ~ Eval (DeleteDims ds1 s1),
    si ~ Eval (GetDims ds0 s0),
    si ~ Eval (GetDims ds1 s1),
    st ~ Eval ((++) so0 so1),
    ds0 ~ '[Eval ((Fcf.-) (Eval (Rank s0)) 1)],
    ds1 ~ '[0]
  ) =>
  Array s0 a ->
  Array s1 a ->
  Array st a
mult = dot sum (*)

instance
  ( Multiplicative a,
    P.Distributive a,
    Subtractive a,
    KnownNat m
  ) =>
  Multiplicative (Matrix m m a)
  where
  (*) = mult

  one = ident

instance
  ( Multiplicative a,
    P.Distributive a,
    Subtractive a,
    Eq a,
    ExpField a,
    KnownNat m
  ) =>
  Divisive (Matrix m m a)
  where
  recip a = invtri (transpose (chol a)) * invtri (chol a)

-- | Inverse of a square matrix.
--
-- > mult (inverse a) a == a
--
-- >>> e = array @[3,3] @Double [4,12,-16,12,37,-43,-16,-43,98]
-- >>> pretty (inverse e)
-- [[49.36111111111111,-13.555555555555554,2.1111111111111107],
--  [-13.555555555555554,3.7777777777777772,-0.5555555555555555],
--  [2.1111111111111107,-0.5555555555555555,0.1111111111111111]]
inverse :: (Eq a, ExpField a, KnownNat m) => Matrix m m a -> Matrix m m a
inverse a = mult (invtri (transpose (chol a))) (invtri (chol a))

-- | [Inversion of a Triangular Matrix](https://math.stackexchange.com/questions/1003801/inverse-of-an-invertible-upper-triangular-matrix-of-order-3)
--
-- >>> t = array @[3,3] @Double [1,0,1,0,1,2,0,0,1]
-- >>> pretty (invtri t)
-- [[1.0,0.0,-1.0],
--  [0.0,1.0,-2.0],
--  [0.0,0.0,1.0]]
--
-- > ident == mult t (invtri t)
-- True
invtri :: forall a n. (KnownNat n, ExpField a, Eq a) => Matrix n n a -> Matrix n n a
invtri a = sum (fmap (l ^) (iota @n)) * ti
  where
    ti = undiag (fmap recip (diag a))
    tl = a - undiag (diag a)
    l = negate (ti * tl)

-- | cholesky decomposition
--
-- Uses the <https://en.wikipedia.org/wiki/Cholesky_decomposition#The_Cholesky_algorithm Cholesky-Crout> algorithm.
--
-- >>> e = array @[3,3] @Double [4,12,-16,12,37,-43,-16,-43,98]
-- >>> pretty (chol e)
-- [[2.0,0.0,0.0],
--  [6.0,1.0,0.0],
--  [-8.0,5.0,3.0]]
-- >>> mult (chol e) (F.transpose (chol e)) == e
-- True
chol :: (KnownNat m, ExpField a) => Matrix m m a -> Matrix m m a
chol a =
  let l =
        unsafeTabulate
          ( \[i, j] ->
              bool
                ( one
                    / unsafeIndex l [j, j]
                    * ( unsafeIndex a [i, j]
                          - sum
                            ( (\k -> unsafeIndex l [i, k] * unsafeIndex l [j, k])
                                <$> ([0 .. (j - 1)] :: [Int])
                            )
                      )
                )
                ( sqrt
                    ( unsafeIndex a [i, i]
                        - sum
                          ( (\k -> unsafeIndex l [j, k] ^ (2 :: Int))
                              <$> ([0 .. (j - 1)] :: [Int])
                          )
                    )
                )
                (i == j)
          )
   in l
