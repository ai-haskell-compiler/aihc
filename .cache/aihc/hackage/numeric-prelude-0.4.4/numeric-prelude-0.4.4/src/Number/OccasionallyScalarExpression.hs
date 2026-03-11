{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Physical expressions track the operations made on physical values
so we are able to give detailed information on how to resolve
unit violations.
-}

module Number.OccasionallyScalarExpression where

import qualified Algebra.Transcendental      as Trans
import qualified Algebra.Algebraic           as Algebraic
import qualified Algebra.Field               as Field
import qualified Algebra.Absolute                as Absolute
import qualified Algebra.Ring                as Ring
import qualified Algebra.Additive            as Additive
import qualified Algebra.ZeroTestable        as ZeroTestable

import qualified Algebra.OccasionallyScalar as OccScalar

import Data.Maybe(fromMaybe)
import Data.Array(listArray,(!))

import NumericPrelude.Base
import NumericPrelude.Numeric


{- | A value of type 'T' stores information on how to resolve unit violations.
     The main application of the module are certainly
     Number.Physical type instances
     but in principle it can also be applied to other occasionally scalar types. -}
data T a v = Cons (Term a v) v

data Term a v =
     Const
   | Add (T a v) (T a v)
   | Mul (T a v) (T a v)
   | Div (T a v) (T a v)

fromValue :: v -> T a v
fromValue = Cons Const


makeLine :: Int -> String -> String
makeLine indent str = replicate indent ' ' ++ str ++ "\n"

showUnitError :: (Show v) => Bool -> Int -> v -> T a v -> String
showUnitError divide indent x (Cons expr y) =
  let indent'   = indent+2
      showSub d = showUnitError d (indent'+2) x
      mulDivArr = listArray (False, True) ["multiply", "divide"]
  in  makeLine indent
         (mulDivArr ! divide ++
          " " ++ show y ++ " by " ++ show x) ++
      case expr of
        (Const) -> ""
        (Add y0 y1) ->
          makeLine indent' "e.g." ++
          showSub divide y0 ++
          makeLine indent' "and " ++
          showSub divide y1
        (Mul y0 y1) ->
          makeLine indent' "e.g." ++
          showSub divide y0 ++
          makeLine indent' "or  " ++
          showSub divide y1
        (Div y0 y1) ->
          makeLine indent' "e.g." ++
          showSub divide y0 ++
          makeLine indent' "or  " ++
          showSub (not divide) y1


lift :: (v -> v) -> (T a v -> T a v)
lift f (Cons xe x) = Cons xe (f x)

fromScalar :: (Show v, OccScalar.C a v) =>
   a -> T a v
fromScalar = OccScalar.fromScalar

scalarMap :: (Show v, OccScalar.C a v) =>
   (a -> a) -> (T a v -> T a v)
scalarMap f x = OccScalar.fromScalar (f (OccScalar.toScalar x))

scalarMap2 :: (Show v, OccScalar.C a v) =>
   (a -> a -> a) -> (T a v -> T a v -> T a v)
scalarMap2 f x y = OccScalar.fromScalar (f (OccScalar.toScalar x) (OccScalar.toScalar y))


instance (Show v) => Show (T a v) where
  show (Cons _ x) = show x

instance (Eq v) => Eq (T a v) where
  (Cons _ x) == (Cons _ y) = x==y

instance (Ord v) => Ord (T a v) where
  compare (Cons _ x) (Cons _ y) = compare x y

instance (Additive.C v) => Additive.C (T a v) where
  zero = Cons Const zero
  xe@(Cons _ x) + ye@(Cons _ y) = Cons (Add xe ye) (x+y)
  xe@(Cons _ x) - ye@(Cons _ y) = Cons (Add xe ye) (x-y)
  negate = lift negate

instance (Ring.C v) => Ring.C (T a v) where
  xe@(Cons _ x) * ye@(Cons _ y) = Cons (Mul xe ye) (x*y)

  fromInteger = fromValue . fromInteger

instance (Field.C v) => Field.C (T a v) where
  xe@(Cons _ x) / ye@(Cons _ y) = Cons (Div xe ye) (x/y)
  fromRational' = fromValue . fromRational'

instance (ZeroTestable.C v) => ZeroTestable.C (T a v) where
  isZero (Cons _ x) = isZero x

instance (Absolute.C v) => Absolute.C (T a v) where
  {- are these definitions sensible? -}
  abs    = lift abs
  signum = lift signum


{- This instance is not quite satisfying.
   The expression data structure should also keep track of powers
   in order to report according errors. -}
instance (Algebraic.C a, Field.C v, Show v, OccScalar.C a v) =>
    Algebraic.C (T a v) where
  sqrt    = scalarMap  sqrt
  x ^/ y  = scalarMap  (^/ y) x

instance (Trans.C a, Field.C v, Show v, OccScalar.C a v) =>
    Trans.C (T a v) where
  pi      = fromScalar pi
  log     = scalarMap  log
  exp     = scalarMap  exp
  logBase = scalarMap2 logBase
  (**)    = scalarMap2 (**)
  cos     = scalarMap  cos
  tan     = scalarMap  tan
  sin     = scalarMap  sin
  acos    = scalarMap  acos
  atan    = scalarMap  atan
  asin    = scalarMap  asin
  cosh    = scalarMap  cosh
  tanh    = scalarMap  tanh
  sinh    = scalarMap  sinh
  acosh   = scalarMap  acosh
  atanh   = scalarMap  atanh
  asinh   = scalarMap  asinh


instance (OccScalar.C a v, Show v)
      => OccScalar.C a (T a v) where
   toScalar xe@(Cons _ x) =
      fromMaybe
         (error (show xe ++ " is not a scalar value.\n" ++
                 showUnitError True 0 x xe))
         (OccScalar.toMaybeScalar x)
   toMaybeScalar (Cons _ x) = OccScalar.toMaybeScalar x
   fromScalar = fromValue . OccScalar.fromScalar


{-
  I would like to use OccasionallyScalar.toScalar
  in fmap and (>>=) to allow more sophisticated error messages
  for types that support more descriptive error messages.
  But this requires constraints to the type arguments of
  Functor and Monad.
-}


{- Operators for lifting scalar operations to
   operations on physical values -}
{-
instance Functor (T i) where
  fmap f (Cons xu x) =
    if Unit.isScalar xu
    then OccScalar.fromScalar (f x)
    else error "Physics.Quantity.Value.fmap: function for scalars, only"

instance Monad (T i) where
  (>>=) (Cons xu x) f =
    if Unit.isScalar xu
    then f x
    else error "Physics.Quantity.Value.(>>=): function for scalars, only"
  return = OccScalar.fromScalar
-}
