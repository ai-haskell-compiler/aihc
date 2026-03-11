{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  Prelude.InfBackprop
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Backpropagation differentiable versions of basic functions.
module Prelude.InfBackprop
  ( -- * Elementary functions
    linear,
    (+),
    (-),
    negate,
    (*),
    (/),

    -- * Tuple manipulations
    dup,
    setFirst,
    setSecond,
    forget,
    forgetFirst,
    forgetSecond,

    -- * Exponential family functions
    log,
    logBase,
    exp,
    (**),
    pow,

    -- * Trigonometric functions
    cos,
    sin,
    tan,
    asin,
    acos,
    atan,
    atan2,
    sinh,
    cosh,
    tanh,
    asinh,
    acosh,
    atanh,

    -- * Tools
    simpleDifferentiable,
  )
where

import Control.CatBifunctor (first, second, (***))
import Control.Category ((<<<), (>>>))
import InfBackprop.Common (Backprop (MkBackprop), BackpropFunc, const)
import IsomorphismClass.Isomorphism (iso)
import NumHask (Additive, Distributive, Divisive, ExpField, Subtractive, TrigField, fromInteger, zero)
import qualified NumHask as NH
import NumHask.Prelude (one)
import qualified NumHask.Prelude as NHP
import Prelude (flip, uncurry, ($), (==))
import qualified Prelude as P

-- | Returns a differentiable morphism given forward function and backpropagation derivative differential morphism.
--
-- ==== __Examples of usage__
--
-- >>> import qualified NumHask as NH
-- >>> cos = simpleDifferentiable NH.cos (sin >>> negate)
simpleDifferentiable :: forall x. Distributive x => (x -> x) -> BackpropFunc x x -> BackpropFunc x x
simpleDifferentiable f df = MkBackprop call' forward' backward'
  where
    call' :: x -> x
    call' = f

    forward' :: BackpropFunc x (x, x)
    forward' = dup >>> first (simpleDifferentiable f df)

    backward' :: BackpropFunc (x, x) x
    backward' = second df >>> (*)

-- Tuple manipulations

-- | Duplication differentiable operation.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call dup (42.0 :: Float)
-- (42.0,42.0)
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> derivative (dup >>> (*)) x
-- (1·x)+(1·x)
dup ::
  forall x.
  Additive x =>
  BackpropFunc x (x, x)
dup = MkBackprop call' forward' backward'
  where
    call' :: x -> (x, x)
    call' x = (x, x)
    forward' :: BackpropFunc x ((x, x), ())
    forward' = dup >>> (iso :: BackpropFunc y (y, ()))
    backward' :: BackpropFunc ((x, x), ()) x
    backward' = (iso :: BackpropFunc (y, ()) y) >>> (+)

-- | Transforms any function to unit @()@.
-- It is not differentiable until @StartBackprop@ is defined for @()@.
-- However 'forget' is useful if need to remove some data in the differentiable pipeline.
--
-- ==== __Examples of usage__
--
-- >>> import InfBackprop (call, derivative)
--
-- >>> f = first forget >>> (iso :: BackpropFunc ((), a) a) :: Additive a => BackpropFunc (a, a) a
--
-- >>> call f (24, 42)
-- 42
--
-- >>> derivative f (24, 42)
-- (0,1)
forget ::
  forall x.
  Additive x =>
  BackpropFunc x ()
forget = const ()

-- | Remove the first element of a tuple.
--
-- ==== __Examples of usage__
--
-- >>> import InfBackprop (call, derivative)
--
-- >>> call forgetFirst (24, 42)
-- 42
--
-- >>> derivative forgetFirst (24, 42)
-- (0,1)
forgetFirst ::
  forall x y.
  Additive x =>
  BackpropFunc (x, y) y
forgetFirst = iso <<< first forget

-- | Remove the second element of a tuple.
--
-- ==== __Examples of usage__
--
-- >>> import InfBackprop (call, derivative)
--
-- >>> call forgetSecond (24, 42)
-- 24
--
-- >>> derivative forgetSecond (24, 42)
-- (1,0)
forgetSecond ::
  forall x y.
  Additive y =>
  BackpropFunc (x, y) x
forgetSecond = iso <<< second forget

-- | Transforms a 2-argument differentiable function into a single argument function by fixing its first argument.
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call (setFirst 8 (/)) 4 :: Float
-- 2.0
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> derivative (setFirst x (*)) y
-- 1·x
setFirst ::
  forall x y c.
  Additive c =>
  c ->
  BackpropFunc (c, x) y ->
  BackpropFunc x y
setFirst c f = (iso :: BackpropFunc x ((), x)) >>> first (const c) >>> f

-- | Transforms a 2-argument differentiable function into a single argument function by fixing its second argument.
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call (setSecond 4 (/)) 8 :: Float
-- 2.0
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> derivative (setSecond y (*)) x
-- 1·y
setSecond ::
  forall x y c.
  Additive c =>
  c ->
  BackpropFunc (x, c) y ->
  BackpropFunc x y
setSecond c f = (iso :: BackpropFunc x (x, ())) >>> second (const c) >>> f

-- Elementary functions

-- | Linear differentiable function.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (fmap, Float)
-- >>> import InfBackprop (pow, call, derivative)
-- >>> myFunc = linear 2 :: BackpropFunc Float Float
--
-- >>> f = call myFunc :: Float -> Float
-- >>> fmap f [-3, -2, -1, 0, 1, 2, 3]
-- [-6.0,-4.0,-2.0,0.0,2.0,4.0,6.0]
--
-- >>> df = derivative myFunc :: Float -> Float
-- >>> fmap df [-3, -2, -1, 0, 1, 2, 3]
-- [2.0,2.0,2.0,2.0,2.0,2.0,2.0]
linear ::
  forall x.
  NH.Distributive x =>
  x ->
  BackpropFunc x x
linear c = MkBackprop call' forward' backward'
  where
    call' :: x -> x
    call' = f c
      where
        f = (NH.*)
    forward' :: BackpropFunc x (x, ())
    forward' = linear c >>> (iso :: BackpropFunc y (y, ()))
    backward' :: BackpropFunc (x, ()) x
    backward' = (iso :: BackpropFunc (x, ()) x) >>> linear c

-- | Summation differentiable binary operation.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
--
-- >>> call (+) (2, 3) :: Float
-- 5.0
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> derivative (+) (x, y)
-- (1,1)
(+) ::
  forall x.
  Additive x =>
  BackpropFunc (x, x) x
(+) = MkBackprop call' forward' backward'
  where
    call' :: (x, x) -> x
    call' = uncurry (NH.+)
    forward' :: BackpropFunc (x, x) (x, ())
    forward' = (+) >>> (iso :: BackpropFunc y (y, ()))
    backward' :: BackpropFunc (x, ()) (x, x)
    backward' = (iso :: BackpropFunc (x, ()) x) >>> dup

-- | Negate differentiable function.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float, ($))
-- >>> import InfBackprop (call, derivative)
--
-- >>> call negate 42 :: Float
-- -42.0
--
-- >>> derivative negate 42 :: Float
-- -1.0
negate ::
  forall x.
  Subtractive x =>
  BackpropFunc x x
negate = MkBackprop call' forward' backward'
  where
    call' :: x -> x
    call' = NH.negate
    forward' :: BackpropFunc x (x, ())
    forward' = negate >>> (iso :: BackpropFunc y (y, ()))
    backward' :: BackpropFunc (x, ()) x
    backward' = (iso :: BackpropFunc (y, ()) y) >>> negate

-- | Subtraction differentiable binary operation.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
--
-- >>> call (-) (5, 3) :: Float
-- 2.0
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> derivative (-) (x, y)
-- (1,-(1))
(-) :: forall x. (Subtractive x) => BackpropFunc (x, x) x
(-) = MkBackprop call' forward' backward'
  where
    call' :: (x, x) -> x
    call' = uncurry (NH.-)
    forward' :: BackpropFunc (x, x) (x, ())
    forward' = (-) >>> (iso :: BackpropFunc y (y, ()))
    backward' :: BackpropFunc (x, ()) (x, x)
    backward' = (iso :: BackpropFunc (x, ()) x) >>> dup >>> second negate

-- | Product binnary operation
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call (*) (2, 3) :: Float
-- 6.0
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> derivative (*) (x, y)
-- (1·y,1·x)
(*) :: Distributive x => BackpropFunc (x, x) x
(*) = MkBackprop call' forward' backward'
  where
    call' :: Distributive x => (x, x) -> x
    call' = uncurry (NH.*)
    forward' :: Distributive x => BackpropFunc (x, x) (x, (x, x))
    forward' = dup >>> first (*)
    backward' :: Distributive x => BackpropFunc (x, (x, x)) (x, x)
    backward' =
      first dup
        >>> (iso :: BackpropFunc ((dy, dy), (x1, x2)) ((dy, x1), (dy, x2)))
        >>> (iso :: BackpropFunc (a, b) (b, a))
        >>> (*) *** (*)

-- | Square differentiable operation
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call square 3 :: Float
-- 9.0
--
-- >>> derivative square 3 :: Float
-- 6.0
square :: Distributive x => BackpropFunc x x
square = dup >>> (*)

-- | Division binary differentiable operation
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call (/) (6, 3) :: Float
-- 2.0
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> derivative (/) (x, y)
-- (1·(1/y),1·(-(x)·(1/(y·y))))
(/) ::
  forall x.
  (Divisive x, Distributive x, Subtractive x) =>
  BackpropFunc (x, x) x
(/) = MkBackprop call' forward' backward'
  where
    call' :: (x, x) -> x
    call' = uncurry (NH./)
    forward' :: BackpropFunc (x, x) (x, (x, x))
    forward' = dup >>> first (/)
    backward' :: BackpropFunc (x, (x, x)) (x, x)
    backward' =
      dup *** dup
        >>> second (d1 *** d2) -- ((dy, dy), ((x1, x2), (x1, x2)))
        >>> (iso :: BackpropFunc ((dy, dy), (x1, x2)) ((dy, x1), (dy, x2))) -- ((dy, dy), (1 / x2, - x1 * x2^(-2) ))
        >>> (*) *** (*)
      where
        d1 = (forget *** recip) >>> (iso :: BackpropFunc ((), x) x) -- (x1, x2) -> 1 / x2
        d2 = (negate *** (square >>> recip)) >>> (*) -- (x1, x2) -> - x1 * x2^(-2)

-- | The recip differentiable operation
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call recip 2 :: Float
-- 0.5
--
-- >>> derivative recip 2 :: Float
-- -0.25
recip ::
  forall x.
  (Divisive x, Distributive x, Subtractive x) =>
  BackpropFunc x x
recip = setFirst NH.one (/)

-- | Integer power differentiable operation
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call (pow 3) 2 :: Float
-- 8.0
--
-- >>> derivative (pow 3) 2 :: Float
-- 12.0
pow ::
  forall x.
  ( Divisive x,
    Distributive x,
    Subtractive x,
    NH.FromIntegral x NHP.Integer
  ) =>
  NHP.Integer ->
  BackpropFunc x x
pow n = MkBackprop call' forward' backward'
  where
    call' :: x -> x
    call' = flip (NH.^) (fromInteger n)
    forward' :: BackpropFunc x (x, x)
    forward' = dup >>> first (pow n :: BackpropFunc x x)
    backward' :: BackpropFunc (x, x) x
    backward' = second der >>> (*)
      where
        der =
          if n == 0
            then const zero
            else pow (n P.- 1) >>> linear (NH.fromIntegral n)

-- | Square root differentiable function.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call sqrt 16 :: Float
-- 4.0
--
-- >>> derivative sqrt 16 :: Float
-- 0.125
sqrt ::
  forall x.
  ExpField x =>
  BackpropFunc x x
sqrt = MkBackprop call' forward' backward'
  where
    call' :: x -> x
    call' = NH.sqrt
    forward' :: BackpropFunc x (x, x)
    forward' = (sqrt :: BackpropFunc x x) >>> dup
    backward' :: BackpropFunc (x, x) x
    backward' = second (recip >>> linear NH.half) >>> (*)

-- | Power binary differentiable operation.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import NumHask (half)
-- >>> import InfBackprop (call, derivative)
-- >>> call (**) (0.5, 9) :: Float
-- 3.0
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> n = variable "n"
-- >>> derivative (**) (n, x)
-- (1·(n·(x^(n-1))),1·((x^n)·log(x)))
(**) ::
  forall a.
  ( ExpField a,
    NH.FromIntegral a P.Integer
  ) =>
  BackpropFunc (a, a) a
(**) = MkBackprop call' forward' backward'
  where
    call' :: (a, a) -> a
    call' = uncurry $ flip (NH.**)
    forward' :: BackpropFunc (a, a) (a, (a, (a, a)))
    forward' =
      dup -- ((n, x), (n, x))
        >>> first ((**) >>> dup) -- ((x^n, x^n), (n, x))
        >>> (iso :: BackpropFunc ((a, b), c) (a, (b, c))) -- (x^n, (x^n, (n, x)))
    backward' :: BackpropFunc (a, (a, (a, a))) (a, a)
    backward' =
      dup *** (dup >>> (dn *** dx)) -- ((dy, dy), (dn, dx))
        >>> (iso :: BackpropFunc ((a, b), (c, d)) ((a, c), (b, d))) -- ((dy, dn), (dy, dx))
        >>> (*) *** (*)
      where
        -- (x^n, (n, x)) -> n * x^(n-1)
        dn :: BackpropFunc (a, (a, a)) a
        dn =
          forgetFirst -- (n, x)
            >>> first dup -- ((n, n), x)
            >>> (iso :: BackpropFunc ((a, b), c) (a, (b, c))) -- (n, (n, x))
            >>> second (first (setSecond (NH.fromIntegral (1 :: P.Integer)) (-))) -- (n, (n-1, x))
            >>> second (**) -- (n, x^(n-1))
            >>> (*) -- (n * x^(n-1))
            -- (x^n, (n, x)) -> log x * x^n
        dx :: BackpropFunc (a, (a, a)) a
        dx = second forgetFirst >>> second log >>> (*)

-- | Natural logarithm differentiable function.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call log 10 :: Float
-- 2.3025851
--
-- >>> derivative log 10 :: Float
-- 0.1
log :: ExpField x => BackpropFunc x x
log = simpleDifferentiable NH.log recip

-- | Natural logarithm differentiable function.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call logBase (2, 8) :: Float
-- 3.0
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> n = variable "n"
-- >>> derivative logBase (n, x)
-- ((1·(-(log(x))·(1/(log(n)·log(n)))))·(1/n),(1·(1/log(n)))·(1/x))
logBase :: ExpField a => BackpropFunc (a, a) a
logBase = (iso :: BackpropFunc (c, d) (d, c)) >>> log *** log >>> (/)

-- | Natural logarithm differentiable function.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative)
-- >>> call exp 2
-- 7.38905609893065
--
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> x = variable "x"
-- >>> derivative exp x
-- 1·exp(x)
exp :: forall x. ExpField x => BackpropFunc x x
exp = MkBackprop call' forward' backward'
  where
    call' :: x -> x
    call' = NH.exp
    forward' :: BackpropFunc x (x, x)
    forward' = (exp :: BackpropFunc x x) >>> dup
    backward' :: BackpropFunc (x, x) x
    backward' = (*)

-- Trigonometric

-- | Sine differentiable function
sin :: TrigField x => BackpropFunc x x
sin = simpleDifferentiable NH.sin cos

-- | Cosine differentiable function.
cos :: TrigField x => BackpropFunc x x
cos = simpleDifferentiable NH.cos (sin >>> negate)

-- | Tangent differentiable function.
tan :: TrigField x => BackpropFunc x x
tan = simpleDifferentiable NH.tan (cos >>> square >>> recip)

-- | Arcsine differentiable function.
asin :: (TrigField x, ExpField x) => BackpropFunc x x
asin = simpleDifferentiable NH.tan (square >>> setFirst one (-) >>> sqrt >>> recip)

-- | Arccosine differentiable function.
acos :: (TrigField x, ExpField x) => BackpropFunc x x
acos = simpleDifferentiable NH.tan (square >>> setFirst one (-) >>> sqrt >>> recip >>> negate)

-- | Arctangent differentiable function.
atan :: TrigField x => BackpropFunc x x
atan = simpleDifferentiable NH.atan (square >>> setFirst one (+) >>> recip)

-- | 2-argument arctangent differentiable function.
atan2 :: TrigField a => BackpropFunc (a, a) a
atan2 = (/) >>> atan

-- | Hyperbolic sine differentiable function.
sinh :: TrigField x => BackpropFunc x x
sinh = simpleDifferentiable NH.sinh cosh

-- | Hyperbolic cosine differentiable function.
cosh :: TrigField x => BackpropFunc x x
cosh = simpleDifferentiable NH.cosh sinh

-- | Hyperbolic tanget differentiable function.
tanh :: TrigField x => BackpropFunc x x
tanh = simpleDifferentiable NH.tanh (cosh >>> square >>> recip)

-- | Hyperbolic arcsine differentiable function.
asinh :: (TrigField x, ExpField x) => BackpropFunc x x
asinh = simpleDifferentiable NH.asinh (square >>> setFirst one (+) >>> sqrt >>> recip)

-- | Hyperbolic arccosine differentiable function.
acosh :: (TrigField x, ExpField x) => BackpropFunc x x
acosh = simpleDifferentiable NH.tan (square >>> setSecond one (-) >>> sqrt >>> recip)

-- | Hyperbolic arctangent differentiable function.
atanh :: TrigField x => BackpropFunc x x
atanh = simpleDifferentiable NH.tan (square >>> setFirst one (-) >>> recip)
