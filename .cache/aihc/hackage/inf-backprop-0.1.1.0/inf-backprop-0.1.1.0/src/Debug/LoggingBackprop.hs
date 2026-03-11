{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  Debug.LoggingBackprop
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Basics for simple expressions equipped with Monadic behaviour.
-- In particular, basic functions with logging for debug and illustration purposes.
-- See [this tutorial section](InfBackprop.Tutorial#differentiation_monadic_types) for details.
module Debug.LoggingBackprop
  ( -- * Generic logging functions
    unitConst,
    initUnaryFunc,
    initBinaryFunc,
    pureKleisli,
    backpropExpr,
    loggingBackpropExpr,

    -- * Logging functions examples
    const,
    linear,
    negate,
    (+),
    (*),
    pow,
    exp,
    sin,
    cos,
  )
where

import Control.Arrow (Kleisli (Kleisli))
import Control.CatBifunctor (first, second, (***))
import Control.Category ((.), (>>>))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Text (pack)
import Debug.SimpleExpr.Expr (SimpleExpr, unaryFunc)
import InfBackprop.Common (Backprop (MkBackprop), BackpropFunc)
import IsomorphismClass.Isomorphism (iso)
import NumHask (Additive, Distributive, Divisive, ExpField, Multiplicative, Subtractive, TrigField, fromInteger, zero)
import qualified NumHask as NH
import qualified NumHask.Prelude as NHP
import qualified Prelude.InfBackprop
import Prelude (Monad, Show, String, pure, return, show, ($), (<>))
import qualified Prelude as P

-- | Logging constant function.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
--
-- >>> runStdoutLoggingT $ runKleisli (unitConst 42) ()
-- [Info] Initializing 42
-- 42
unitConst :: (Show a, MonadLogger m) => a -> Kleisli m () a
unitConst a = Kleisli $ \() -> do
  logInfoN $ "Initializing " <> pack (show a)
  pure a

-- | Logging single argument function.
--
-- ==== __Examples of usage__
--
-- >>> import qualified Prelude as P
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
--
-- >>> plusTwo = initUnaryFunc "+2" (P.+2)
-- >>> runStdoutLoggingT $ runKleisli plusTwo 3
-- [Info] Calculating +2 of 3 => 5
-- 5
initUnaryFunc :: (Show a, Show b, MonadLogger m) => String -> (a -> b) -> Kleisli m a b
initUnaryFunc msg f = Kleisli $ \a -> do
  let b = f a
  logInfoN $ "Calculating " <> pack msg <> " of " <> pack (show a) <> " => " <> pack (show b)
  pure b

-- | Logging two argument (binary) function.
--
-- ==== __Examples of usage__
--
-- >>> import qualified Prelude as P
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
--
-- >>> loggingProduct = initBinaryFunc "product" (P.*)
-- >>> runStdoutLoggingT $ runKleisli loggingProduct (6, 7)
-- [Info] Calculating product of 6 and 7 => 42
-- 42
initBinaryFunc :: (Show a, Show b, Show c, MonadLogger m) => String -> (a -> b -> c) -> Kleisli m (a, b) c
initBinaryFunc msg f = Kleisli $ \(a, b) -> do
  let c = f a b
  logInfoN $
    "Calculating "
      <> pack msg
      <> " of "
      <> pack (show a)
      <> " and "
      <> pack (show b)
      <> " => "
      <> pack (show c)
  return c

-- | Returns pure Kleisli morphism given a map.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
--
-- >>> loggingDup = pureKleisli (\x -> (x, x))
-- >>> runStdoutLoggingT $ runKleisli loggingDup 42
-- (42,42)
pureKleisli :: Monad m => (a -> b) -> Kleisli m a b
pureKleisli f = Kleisli $ pure . f

-- Differentiable functions.

-- | Returns symbolically differentiable Simple Expression.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> import InfBackprop (call, derivative, backpropExpr)
--
-- >>> x = variable "x"
-- >>> f = backpropExpr "f"
-- >>> call f x
-- f(x)
--
-- >>> derivative f x
-- 1·f'(x)
backpropExpr :: String -> BackpropFunc SimpleExpr SimpleExpr
backpropExpr funcName = MkBackprop call_ forward_ backward_
  where
    call_ = unaryFunc funcName
    forward_ = Prelude.InfBackprop.dup >>> first (backpropExpr funcName :: BackpropFunc SimpleExpr SimpleExpr)
    backward_ = second (backpropExpr (funcName <> "'")) >>> (Prelude.InfBackprop.*)

-- | Returns symbolically differentiable logging symbolic function.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> import InfBackprop (call, derivative)
--
-- >>> x = variable "x"
-- >>> f = loggingBackpropExpr "f"
-- >>> runStdoutLoggingT $ runKleisli (call f) x
-- [Info] Calculating f of x => f(x)
-- f(x)
--
-- >>> runStdoutLoggingT $ runKleisli (derivative f) x
-- [Info] Calculating f of x => f(x)
-- [Info] Calculating f' of x => f'(x)
-- [Info] Calculating multiplication of 1 and f'(x) => 1·f'(x)
-- 1·f'(x)
loggingBackpropExpr :: forall m. (MonadLogger m) => String -> Backprop (Kleisli m) SimpleExpr SimpleExpr
loggingBackpropExpr funcName = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m SimpleExpr SimpleExpr
    call' = initUnaryFunc funcName (unaryFunc funcName)

    forward' :: Backprop (Kleisli m) SimpleExpr (SimpleExpr, SimpleExpr)
    forward' = dup >>> first (loggingBackpropExpr funcName :: Backprop (Kleisli m) SimpleExpr SimpleExpr)

    backward' :: Backprop (Kleisli m) (SimpleExpr, SimpleExpr) SimpleExpr
    backward' = second (loggingBackpropExpr (funcName <> "'")) >>> (*)

-- | Differentiable logging constant function.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
-- >>> import Debug.SimpleExpr.Expr (variable)
-- >>> import InfBackprop (call, derivative)
--
-- >>> runStdoutLoggingT $ runKleisli (call (const 42)) ()
-- 42
const ::
  forall c x m.
  (Additive c, Additive x, Show c, Show x, Monad m) =>
  c ->
  Backprop (Kleisli m) x c
const c = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m x c
    call' = Kleisli $ P.const (pure c)

    forward' :: Backprop (Kleisli m) x (c, ())
    forward' = const c >>> (iso :: Backprop (Kleisli m) c (c, ()))

    backward' :: Backprop (Kleisli m) (c, ()) x
    backward' = const zero

-- | Differentiable dup logging function.
dup :: forall x m. (Show x, Additive x, MonadLogger m) => Backprop (Kleisli m) x (x, x)
dup = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m x (x, x)
    call' = pureKleisli (\x -> (x, x))

    forward' :: Backprop (Kleisli m) x ((x, x), ())
    forward' = dup >>> (iso :: Backprop (Kleisli m) y (y, ()))

    backward' :: Backprop (Kleisli m) ((x, x), ()) x
    backward' = (iso :: Backprop (Kleisli m) (y, ()) y) >>> (+)

-- | Differentiable logging sum function.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
-- >>> import InfBackprop (call)
--
-- >>> runStdoutLoggingT $ runKleisli (call (+)) (2, 2)
-- [Info] Calculating sum of 2 and 2 => 4
-- 4
(+) :: forall x m. (Show x, Additive x, MonadLogger m) => Backprop (Kleisli m) (x, x) x
(+) = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m (x, x) x
    call' = initBinaryFunc "sum" (NH.+)

    forward' :: Backprop (Kleisli m) (x, x) (x, ())
    forward' = (+) >>> (iso :: Backprop (Kleisli m) y (y, ()))

    backward' :: Backprop (Kleisli m) (x, ()) (x, x)
    backward' = (iso :: Backprop (Kleisli m) (x, ()) x) >>> dup

-- | Differentiable logging multiplication function.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Control.Monad.Logger (runStdoutLoggingT)
-- >>> import InfBackprop (call)
--
-- >>> runStdoutLoggingT $ runKleisli (call (*)) (6, 7)
-- [Info] Calculating multiplication of 6 and 7 => 42
-- 42
(*) ::
  forall x m.
  (Show x, Additive x, Multiplicative x, MonadLogger m) =>
  Backprop (Kleisli m) (x, x) x
(*) = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m (x, x) x
    call' = initBinaryFunc "multiplication" (NH.*)

    forward' :: Backprop (Kleisli m) (x, x) (x, (x, x))
    forward' = dup >>> first (*)

    backward' :: Backprop (Kleisli m) (x, (x, x)) (x, x)
    backward' =
      first dup
        >>> (iso :: Backprop (Kleisli m) ((dy, dy), (x1, x2)) ((dy, x1), (dy, x2)))
        >>> (iso :: Backprop (Kleisli m) (a, b) (b, a))
        >>> ((*) *** (*))

-- | Differentiable logging linear function.
linear ::
  forall x m.
  (Show x, NH.Distributive x, MonadLogger m) =>
  x ->
  Backprop (Kleisli m) x x
linear c = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m x x
    call' = initUnaryFunc ("linear " <> show c) (c NH.*)

    forward' :: Backprop (Kleisli m) x (x, ())
    forward' = linear c >>> (iso :: Backprop (Kleisli m) y (y, ()))

    backward' :: Backprop (Kleisli m) (x, ()) x
    backward' = (iso :: Backprop (Kleisli m) (x, ()) x) >>> linear c

-- | Differentiable logging negate function.
negate ::
  forall x m.
  (Show x, Subtractive x, MonadLogger m) =>
  Backprop (Kleisli m) x x
negate = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m x x
    call' = initUnaryFunc "negate" NH.negate

    forward' :: Backprop (Kleisli m) x (x, ())
    forward' = negate >>> (iso :: Backprop (Kleisli m) y (y, ()))

    backward' :: Backprop (Kleisli m) (x, ()) x
    backward' = (iso :: Backprop (Kleisli m) (y, ()) y) >>> negate

-- | Differentiable logging exponent function.
exp ::
  forall x m.
  (ExpField x, Show x, MonadLogger m) =>
  Backprop (Kleisli m) x x
exp = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m x x
    call' = initUnaryFunc "exp" NH.exp

    forward' :: Backprop (Kleisli m) x (x, x)
    forward' = (exp :: Backprop (Kleisli m) x x) >>> dup

    backward' :: Backprop (Kleisli m) (x, x) x
    backward' = (*)

-- | Differentiable logging power function.
pow ::
  forall x m.
  (Show x, Divisive x, Distributive x, Subtractive x, NH.FromIntegral x NHP.Integer, MonadLogger m) =>
  NHP.Integer ->
  Backprop (Kleisli m) x x
pow n = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m x x
    call' = initUnaryFunc ("pow " <> show n) (NH.^ fromInteger n)

    forward' :: Backprop (Kleisli m) x (x, x)
    forward' = dup >>> first (pow n :: Backprop (Kleisli m) x x)

    backward' :: Backprop (Kleisli m) (x, x) x
    backward' = second (pow (n P.- 1) >>> linear (NH.fromIntegral n)) >>> (*)

-- | Differentiable logging sin function.
sin ::
  forall x m.
  (Show x, TrigField x, MonadLogger m) =>
  Backprop (Kleisli m) x x
sin = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m x x
    call' = initUnaryFunc "sin" NH.sin

    forward' :: Backprop (Kleisli m) x (x, x)
    forward' = dup >>> first (sin :: Backprop (Kleisli m) x x)

    backward' :: Backprop (Kleisli m) (x, x) x
    backward' = second (cos :: Backprop (Kleisli m) x x) >>> (*)

-- | Differentiable logging cos function.
cos ::
  forall x m.
  (Show x, TrigField x, MonadLogger m) =>
  Backprop (Kleisli m) x x
cos = MkBackprop call' forward' backward'
  where
    call' :: Kleisli m x x
    call' = initUnaryFunc "cos" NH.cos

    forward' :: Backprop (Kleisli m) x (x, x)
    forward' = dup >>> first (sin :: Backprop (Kleisli m) x x)

    backward' :: Backprop (Kleisli m) (x, x) x
    backward' = second (sin >>> negate :: Backprop (Kleisli m) x x) >>> (*)
