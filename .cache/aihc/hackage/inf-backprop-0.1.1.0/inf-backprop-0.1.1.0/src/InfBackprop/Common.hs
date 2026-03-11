{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  InfBackprop.Common
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Provides base types and methods for backpropagation category morphism.
module InfBackprop.Common
  ( -- * Basic
    Backprop (MkBackprop),
    call,
    forward,
    backward,
    StartBackprop,
    startBackprop,
    forwardBackward,
    numba,
    numbaN,
    derivative,
    derivativeN,

    -- * Differentiable functions
    BackpropFunc,
    const,

    -- * Differentiable monadic functions
    pureBackprop,
  )
where

import Control.Arrow (Kleisli (Kleisli))
import Control.CatBifunctor (CatBiFunctor, first, (***))
import Control.Category (Category, id, (.), (>>>))
import GHC.Natural (Natural)
import IsomorphismClass (IsomorphicTo)
import IsomorphismClass.Extra ()
import IsomorphismClass.Isomorphism (Isomorphism, iso)
import NumHask (one, zero)
import NumHask.Algebra.Additive (Additive)
import NumHask.Algebra.Ring (Distributive)
import NumHask.Extra ()
import Prelude (Monad, flip, fromIntegral, iterate, pure, (!!), ($))
import qualified Prelude as P

-- | Backprop morphism.
-- #backprop#
-- Base type for an infinitely differentiable object.
-- It depends on categorical type @cat@ that is mostly common @(->)@,
-- see 'BackpropFunc' which by it's definition is equivalent to
--
-- @
-- data BackpropFunc input output = forall cache. MkBackpropFunc {
--  call     :: input -> output,
--  forward  :: BackpropFunc input (output, cache),
--  backward :: BackpropFunc (output, cache) input
-- }
-- @
--
-- The diagram below illustrates the how it works for the first derivative.
-- Consider the role of function @f@ in the derivative of the composition @g(f(h(...)))@.
-- #backprop_func#
--
-- @
--   h        ·                  f                   ·        g
--            ·                                      ·
--            ·               forward                ·
--            · --- input  >-----+-----> output >--- ·
--            ·                  V                   ·
--  ...       ·                  |                   ·       ...
--            ·                  | cache             ·
--            ·                  |                   ·
--            ·                  V                   ·
--            · --< dInput <-----+-----< dOutput <-- ·
--            ·               backward               ·
-- @
--
-- Notice that 'forward' and 'backward' are of type 'BackpropFunc' but not @(->)@.
-- This is needed for further differentiation.
-- However for the first derivative this difference can be ignored.
--
-- The return type of 'forward' contains additional term @cache@.
-- It is needed to save and transfer data calculated in the forward step to the backward step for reuse.
-- See an example in
--
-- [Differentiation with logging](#differentiation_with_logging)
-- section .
--
-- == __Remark__
-- Mathematically speaking we have to distinguish the types for 'forward' and for 'backward' methods because the second
-- acts on the cotangent bundle.
-- However, for simplicity and due to technical reasons we identify the types @input@ and @dInput@
-- as well as @output@ and @dOutput@ which is enough for our purposes because these types are usually real numbers
-- or arrays of real numbers.
data Backprop cat input output = forall cache.
  MkBackprop
  { -- | Simple internal category object extraction.
    call :: cat input output,
    -- | Returns forward category.
    -- In the case @cat = (->)@, the method coincides with 'Backprop'@ cat input output@ itself
    -- but the output contains an additional data term @cache@ with some calculation result that can be reused on in
    -- 'backward'.
    forward :: Backprop cat input (output, cache),
    -- | Returns backward category. In the case @cat = (->)@, the method takes the additional data term @cache@ that is
    -- calculated in 'forward'.
    backward :: Backprop cat (output, cache) input
  }

composition' ::
  forall cat x y z.
  (Isomorphism cat, CatBiFunctor (,) cat) =>
  Backprop cat x y ->
  Backprop cat y z ->
  Backprop cat x z
composition'
  (MkBackprop callF (forwardF :: Backprop cat x (y, hF)) (backwardF :: Backprop cat (y, hF) x))
  (MkBackprop callG (forwardG :: Backprop cat y (z, hG)) (backwardG :: Backprop cat (z, hG) y)) =
    MkBackprop call_ forward_ backward_
    where
      call_ :: cat x z
      call_ = callF >>> callG

      forward_ :: Backprop cat x (z, (hG, hF))
      forward_ =
        (forwardF `composition'` first forwardG) `composition'` (iso :: Backprop cat ((z, hG), hF) (z, (hG, hF)))

      backward_ :: Backprop cat (z, (hG, hF)) x
      backward_ =
        (iso :: Backprop cat (z, (hG, hF)) ((z, hG), hF)) `composition'` first backwardG `composition'` backwardF

iso' ::
  forall cat x y.
  (IsomorphicTo x y, Isomorphism cat, CatBiFunctor (,) cat) =>
  Backprop cat x y
iso' = MkBackprop call_ (forward_ :: Backprop cat x (y, ())) (backward_ :: Backprop cat (y, ()) x)
  where
    call_ :: cat x y
    call_ = iso

    forward_ :: Backprop cat x (y, ())
    forward_ = (iso :: Backprop cat x y) `composition'` (iso :: Backprop cat y (y, ()))

    backward_ :: Backprop cat (y, ()) x
    backward_ = (iso :: Backprop cat (y, ()) y) `composition'` (iso :: Backprop cat y x)

instance
  (Isomorphism cat, CatBiFunctor (,) cat) =>
  Category (Backprop cat)
  where
  id = iso'
  (.) = flip composition'

instance
  (Isomorphism cat, CatBiFunctor (,) cat) =>
  Isomorphism (Backprop cat)
  where
  iso = iso'

instance
  (Isomorphism cat, CatBiFunctor (,) cat) =>
  CatBiFunctor (,) (Backprop cat)
  where
  (***)
    (MkBackprop call1 (forward1 :: Backprop cat x1 (y1, h1)) (backward1 :: Backprop cat (y1, h1) x1))
    (MkBackprop call2 (forward2 :: Backprop cat x2 (y2, h2)) (backward2 :: Backprop cat (y2, h2) x2)) =
      MkBackprop call12 forward12 backward12
      where
        call12 :: cat (x1, x2) (y1, y2)
        call12 = call1 *** call2

        forward12 :: Backprop cat (x1, x2) ((y1, y2), (h1, h2))
        forward12 = forward1 *** forward2 >>> (iso :: Backprop cat ((y1, h1), (y2, h2)) ((y1, y2), (h1, h2)))

        backward12 :: Backprop cat ((y1, y2), (h1, h2)) (x1, x2)
        backward12 = (iso :: Backprop cat ((y1, y2), (h1, h2)) ((y1, h1), (y2, h2))) >>> backward1 *** backward2

-- | Implementation of the process illustrated in the
-- [diagram](#backprop_func).
-- The first argument is a backprop morphism @y -> dy@
-- The second argument is a backprop morphism @x -> y@
-- The output is the backprop @x -> dx@ build according the
-- [diagram](#backprop_func)
forwardBackward ::
  (Isomorphism cat, CatBiFunctor (,) cat) =>
  -- | backprop morphism between @y@ and @dy@
  -- that is inferred after the forward step for @f@ and before the backward step for @f@
  Backprop cat y y ->
  -- | some backprop morphism @f@ between @x@ and @y@
  Backprop cat x y ->
  -- | the output backprop morphism from @x@ to @dx@ that is the composition.
  Backprop cat x x
forwardBackward dy (MkBackprop _ forward_ backward_) = forward_ >>> first dy >>> backward_

-- | Interface for categories @cat@ and value types @x@ that support starting the backpropagation.
-- For example for @(->)@ and @Float@ we are able to start the backpropagation like
-- @f(g(x))@ -> @1 · f'(g(x)) · ...@
-- because @f@ is a @Float@ valued (scalar) function.
-- Calculating Jacobians is not currently implemented.
class Distributive x => StartBackprop cat x where
  -- | Morphism that connects forward and backward chain.
  -- Usually it is just @1@ that is supposed to be multiplied on the derivative of the top function.
  startBackprop :: Backprop cat x x

-- | Backpropagation derivative in terms of backprop morphisms.
numba ::
  (Isomorphism cat, CatBiFunctor (,) cat, StartBackprop cat y) =>
  Backprop cat x y ->
  Backprop cat x x
numba = forwardBackward startBackprop

-- | Backpropagation ns derivative in terms of backprop morphisms.
numbaN ::
  (Isomorphism cat, CatBiFunctor (,) cat, StartBackprop cat x) =>
  Natural ->
  Backprop cat x x ->
  Backprop cat x x
numbaN n f = iterate numba f !! fromIntegral n

-- | Backpropagation derivative as categorical object.
-- If @cat@ is @(->)@ the output is simply a function.
--
-- ==== __Examples of usage__
--
-- >>> import InfBackprop (sin)
-- >>> import Prelude (Float)
-- >>> derivative sin (0 :: Float)
-- 1.0
derivative ::
  (Isomorphism cat, CatBiFunctor (,) cat, StartBackprop cat y) =>
  Backprop cat x y ->
  cat x x
derivative = call . numba

-- | Backpropagation derivative of order n as categorical object.
-- If @cat@ is @(->)@ the output is simply a function.
--
-- ==== __Examples of usage__
--
-- >>> import InfBackprop (pow, const)
-- >>> import Prelude (Float, fmap)
-- >>> myFunc = (pow 2) :: Backprop (->) Float Float
--
-- >>> fmap (derivativeN 0 myFunc) [-3, -2, -1, 0, 1, 2, 3]
-- [9.0,4.0,1.0,0.0,1.0,4.0,9.0]
--
-- >>> fmap (derivativeN 1 myFunc) [-3, -2, -1, 0, 1, 2, 3]
-- [-6.0,-4.0,-2.0,0.0,2.0,4.0,6.0]
--
-- >>> fmap (derivativeN 2 myFunc) [-3, -2, -1, 0, 1, 2, 3]
-- [2.0,2.0,2.0,2.0,2.0,2.0,2.0]
--
-- >>> fmap (derivativeN 3 myFunc) [-3, -2, -1, 0, 1, 2, 3]
-- [0.0,0.0,0.0,0.0,0.0,0.0,0.0]
derivativeN ::
  (Isomorphism cat, CatBiFunctor (,) cat, StartBackprop cat x) =>
  Natural ->
  Backprop cat x x ->
  cat x x
derivativeN n = call . numbaN n

-- | Infinitely differentiable function.
-- The definition of the type synonym is equivalent to
--
-- @
-- data BackpropFunc input output = forall cache. MkBackpropFunc {
--    call     :: input -> output,
--    forward  :: BackpropFunc input (output, cache),
--    backward :: BackpropFunc (output, cache) input
--  }
-- @
--
-- See 'Backprop' for details.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (fmap, Float)
-- >>> import InfBackprop (pow, call, derivative)
-- >>> myFunc = pow 2 :: BackpropFunc Float Float
-- >>> f = call myFunc :: Float -> Float
-- >>> fmap f [-3, -2, -1, 0, 1, 2, 3]
-- [9.0,4.0,1.0,0.0,1.0,4.0,9.0]
-- >>> df = derivative myFunc :: Float -> Float
-- >>> fmap df [-3, -2, -1, 0, 1, 2, 3]
-- [-6.0,-4.0,-2.0,0.0,2.0,4.0,6.0]
type BackpropFunc = Backprop (->)

instance forall x. (Distributive x) => StartBackprop (->) x where
  startBackprop = const one

-- | Infinitely differentiable constant function.
--
-- === __Examples of usage__
--
-- >>> import Prelude (Float)
-- >>> import InfBackprop (call, derivative, derivativeN)
--
-- >>> call (const 5) ()
-- 5
--
-- >>> derivative (const (5 :: Float)) 42
-- 0
--
-- >>> derivativeN 2 (const (5 :: Float)) 42
-- 0.0
const ::
  forall c x.
  (Additive c, Additive x) =>
  c ->
  BackpropFunc x c
const c = MkBackprop call' forward' backward'
  where
    call' :: x -> c
    call' = P.const c
    forward' :: BackpropFunc x (c, ())
    forward' = const c >>> (iso :: BackpropFunc c (c, ()))
    backward' :: BackpropFunc (c, ()) x
    backward' = (iso :: BackpropFunc (c, ()) c) >>> const zero

-- | Lifts a backprop function morphism to the corresponding pure Kleisli morphism.
pureBackprop :: forall a b m. Monad m => Backprop (->) a b -> Backprop (Kleisli m) a b
pureBackprop
  ( MkBackprop
      (call'' :: a -> b)
      (forward'' :: Backprop (->) a (b, c))
      (backward'' :: Backprop (->) (b, c) a)
    ) =
    MkBackprop call' forward' backward'
    where
      call' :: Kleisli m a b
      call' = Kleisli $ pure . call''

      forward' :: Backprop (Kleisli m) a (b, c)
      forward' = pureBackprop forward''

      backward' :: Backprop (Kleisli m) (b, c) a
      backward' = pureBackprop backward''

instance (Distributive x, Monad m) => StartBackprop (Kleisli m) x where
  startBackprop = pureBackprop startBackprop
