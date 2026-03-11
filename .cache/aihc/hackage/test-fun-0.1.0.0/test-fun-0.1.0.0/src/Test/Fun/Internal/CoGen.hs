{-# LANGUAGE TypeOperators #-}

-- | Random generation of higher-order functions.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
-- It is made available only for debugging.
-- Otherwise, use "Test.Fun".
--
-- If something here seems useful, please open an issue to export it from an
-- external module.
--
-- === Fun fact
--
-- This module only uses an 'Applicative' constraint on the type of
-- generators (which is really QuickCheck's @Gen@).

module Test.Fun.Internal.CoGen where

import Control.Applicative (liftA2, liftA3)

import Test.Fun.Internal.Types

-- * Cogenerators

-- | A \"cogenerator\" of @a@ is a random generator of functions with domain @a@.
-- They are parameterized by a generator in the codomain @r@.
--
-- More generally, we can make cogenerators to generate functions of arbitrary arities;
-- @'Co' gen a r@ is only the type of unary cogenerators.
--
-- > gen r -> gen (a :-> r)         -- Co gen a r
-- > gen r -> gen (a :-> b :-> r)
-- > gen r -> gen (a :-> b :-> c :-> r)
-- > gen r -> gen (a :-> b :-> c :-> d :-> r)
-- >
-- > -- etc.
--
-- === __More details__
--
-- Cogenerators can be composed using 'id' and @('.')@ (the usual combinators
-- for functions).
-- The arity of a cogenerator @f '.' g@ is the sum of the arities of @f@ and @g@.
--
-- @
-- id  ::  forall r. gen r -> gen r  -- 0-ary cogenerator
--
-- -- (1-ary) . (1-ary) = (2-ary)
-- (.) :: (forall r. gen r -> gen (a :-> r)) ->
--        (forall r. gen r -> gen (b :-> r)) ->
--        (forall r. gen r -> gen (a :-> b :-> r))
--
-- -- (2-ary) . (1-ary) = (3-ary)
-- (.) :: (forall r. gen r -> gen (a :-> b :-> r)) ->
--        (forall r. gen r -> gen (c :-> r)) ->
--        (forall r. gen r -> gen (a :-> b :-> c :-> r))
-- @
--
-- Note: the last type parameter @r@ should really be universally quantified
-- (as in the above pseudo type signatures), but instead we use more specialized
-- types to avoid making types higher-ranked.
type Co gen a r = gen r -> gen (a :-> r)

-- | Cogenerator for a type @a@ from a cogenerator for @b@,
-- given an embedding function @(a -> b)@,
-- and a name for that function (used for pretty-printing).
--
-- === __Example__
--
-- The common usage is to construct cogenerators for newtypes.
--
-- @
-- -- Given some cogenerator of Fruit
-- cogenFruit :: 'Co' Gen Fruit r
--
-- -- Wrap Fruit in a newtype
-- newtype Apple = Apple { unApple :: Fruit }
--
-- cogenApple :: 'Co' Gen Apple r
-- cogenApple = 'cogenEmbed' \"unApple\" cogenFruit
-- @
--
-- If @cogenFruit@ generates a function that looks like:
--
-- > \y -> case y :: Fruit of { ... }
--
-- then @cogenApple@ will look like this,
-- where @y@ is replaced with @unApple x@:
--
-- > \x -> case unApple x :: Fruit of { ... }
--
cogenEmbed :: Functor gen => FunName -> (a -> b) -> Co gen b r -> Co gen a r
cogenEmbed fn f cog g = (ToShrink . Apply fn f) <$> cog g

-- | Cogenerator for an integral type.
-- The name of the type is used for pretty-printing.
--
-- === __Example__
--
-- @
-- cogenInteger :: 'Co' Gen 'Integer' r
-- cogenInteger = 'cogenIntegral' \"Integer\"
--
-- cogenInt :: 'Co' Gen 'Int' r
-- cogenInt = 'cogenIntegral' \"Int\"
--
-- cogenWord :: 'Co' Gen 'Word' r
-- cogenWord = 'cogenIntegral' \"Word\"
-- @
cogenIntegral :: (Applicative gen, Integral a) => TypeName -> Co gen a r
cogenIntegral tn = cogenIntegral' tn toInteger

-- | Variant of 'cogenIntegral' with an explicit conversion to 'Integer'.
cogenIntegral' :: Applicative gen => TypeName -> (a -> Integer) -> Co gen a r
cogenIntegral' tn f g = liftA2 (CaseInteger tn f) (genBin g) g

genBin :: Applicative gen => gen r -> gen (Bin r)
genBin g = BinToShrink <$> self where
  self = liftA3 BinAlt (Just <$> g) self self

-- | Extend a cogenerator of functions @(a -> b)@ (i.e., a generator of higher-order
-- functions @((a -> b) -> r)@), applying the function to a given value @a@
-- and inspecting the result with a cogenerator of @b@.
--
-- This is parameterized by a way to generate, shrink, and show values of
-- type @a@ or, more generally, some representation @a0@ of values of type @a@.
--
-- === __Example__
--
-- @
-- -- Assume Chips is some concrete type.
-- concreteChips :: 'Concrete' Chips
--
-- -- Assume we have a cogenerator of Fish.
-- cogenFish :: forall r. Gen r -> Gen (Fish ':->' r)
--
-- -- Then we can use cogenApply to construct this function
-- -- to transform cogenerators of functions (Chips -> Fish).
-- cogenX :: forall r.
--   Chips ->
--   Gen ((Chips -> Fish) ':->' r) ->
--   Gen ((Chips -> Fish) ':->' r)
-- cogenX = 'cogenApply' concreteChips 'id' '.' cogenFish
--
-- -- If we have some inputs...
-- chips1, chips2, chips3 :: Chips
--
-- -- ... we can construct a cogenerator of functions by iterating cogenX.
-- cogenF :: forall r. Gen r -> Gen ((Chips -> Fish) ':->' r)
-- cogenF = cogenX chips1 '.' cogenX chips2 '.' cogenX chips3 '.' 'cogenConst'
-- @
cogenApply :: Functor gen =>
  Concrete a0 {- ^ Shrink and show @a0@. -} ->
  (a0 -> a)   {- ^ Reify to value @a@ (@id@ for simple data types). -} ->
  a0          {- ^ Value to inspect.     -} ->
  gen (b :-> (a -> b) :-> r) {- ^ Cogenerator of @b@ -} ->
  gen ((a -> b) :-> r)
cogenApply w fromRepr x gr = CoApply w x fromRepr <$> gr

-- | The trivial cogenerator which generates a constant function.
cogenConst :: Functor gen => Co gen a r
cogenConst g = Const <$> g

-- | Construct a cogenerator of functions @(a -> b)@ from a cogenerator of @b@,
-- using @gen (Maybe a0)@ to generate random arguments until it returns
-- @Nothing@.
cogenFun :: Monad gen =>
  Concrete a0    {- ^ Shrink and show @a0@.      -} ->
  gen (Maybe a0) {- ^ Generate representations of argument values. -} ->
  (a0 -> a)      {- ^ Interpret a representation @a0@ into a value @a@ (@id@ for simple data types). -} ->
  Co gen b ((a -> b) :-> r) {- ^ Cogenerator of @b@. -} ->
  Co gen (a -> b) r
cogenFun w ga fromRepr cb gr = self where
  self = do
    ma <- ga
    case ma of
      Nothing -> cogenConst gr
      Just a -> cogenApply w fromRepr a (cb self)
