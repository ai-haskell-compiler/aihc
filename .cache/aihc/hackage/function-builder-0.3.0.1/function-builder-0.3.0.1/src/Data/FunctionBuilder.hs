-- | A builder for functions of variable parameters
--
-- 'FunctionBuilder' values can be composed, and eventually /rendered/
-- into a __function__ by 'toFunction'.
--
-- For example the composition of:
--
-- >
-- > fb1 :: FunctionBuilder MyMonoid composeMe (Int -> composeMe)
-- > fb1 = deferred ...
-- >
-- > fb2 :: FunctionBuilder MyMonoid composeMe (String -> composeMe)
-- > fb2 = deferred ...
-- >
-- > fb3 :: FunctionBuilder MyMonoid composeMe (Bool -> composeMe)
-- > fb3 = deferred ...
-- >
-- > fb :: FunctionBuilder MyMonoid composeMe (Int -> String -> Bool -> composeMe)
-- > fb = fb1 . fb2 . fb3
-- >
-- > f :: Int -> String -> Bool -> MyMonoid
-- > f = toFunction fb123
--
-- 'FunctionBuilder's are composed via '.' from 'Category' and '<>' from 'Semigroup'.
--
-- This module provides 'Functor', 'Applicative', 'Monad', 'Semigroup', 'Monoid' and
-- 'Category' instances;
--
-- The smart-constructors 'immediate' and 'deferred' create 'FunctionBuilder's
-- that either write a hard coded constant to the output 'Monoid' or add a function
-- that will be applied to an additional runtime parameter.
--
-- Further /glue-code/ is provided to allow changing the underlying Monoid, see 'bind'.
--
module Data.FunctionBuilder where

import           Control.Category
import           Data.Monoid
import           Data.Semigroup
import           Prelude                 hiding ( id
                                                , (.)
                                                )
import           Data.Tagged

-- | A tricky newtype wrapper around a function that carries out a computation
-- resulting in a monoidal output value that is passed to a continuation.
--
-- Type parameters:
--
-- [@acc@] Type of monoidal value that is build from the parameters of the function
-- returned by 'toFunction'.
-- For example: In a @printf@ style formatting library @acc@ could be 'String'.
--
-- [@next@] The /trick-/ parameter that allows composing @FunctionBuilder@s.
-- Also note that 'FunctionBuilder's are contravarient in this parameter;
-- @next@ is the output of the continuation @acc -> next@, hence this is an
-- /input/ from the perspective of the @FunctionBuilder@.
--
-- [@f_make_next@] This is usually a function type that returns @next@,
-- this is the type of the output function returned by 'toFunction'.
--
--
-- A @FunctionBuilder acc next f@ is a newtype wrapper around functions of type
-- @(acc -> next) -> f@.
--
-- The immediate return value of the function is usually a function type,
-- that takes zero or more arguments: @a_0 -> .. -> a_N -> next@.
--
-- The @FunctionBuilder@s that 'deferred' returns are polymorphic in @next@.
-- And @next@ is the key for composition.
--
-- For example:
--
-- @
-- fb1 :: FunctionBuilder MyMonoid next (Int -> next)
-- fb1 = deferred undefined
--
-- fb2 :: FunctionBuilder MyMonoid next (String -> next)
-- fb2 = deferred undefined
--
-- newtype MyMonoid = MyMonoid () deriving (Semigroup, Monoid)
-- @
--
-- When we /desugar/ with ghci:
--
-- >>> :t (runFunctionBuilder fb1)
-- (runFunctionBuilder fb1) :: (MyMonoid -> next) -> Int -> next
--
-- >>> :t (runFunctionBuilder fb2)
-- (runFunctionBuilder fb2) :: (MyMonoid -> next) -> String -> next
--
-- Composition comes in two flavours:
--
--     (1) By using '.' to add to the accumulator a value passed to an additional argument
--     of the resulting output function (see example below).
--
--     (2) By using '<>' to append a fixed value to the accumulator directly.
--
-- When __composing__ @fb1@ and @fb2@ using '.' we get:
--
-- >>> :t (fb1 . fb2)
-- (fb1 . fb2) :: FunctionBuilder MyMonoid a (Int -> String -> a)
--
--  And desugared:
--
-- >>> :t runFunctionBuilder (fb1 . fb2)
-- runFunctionBuilder (fb1 . fb2) :: (MyMonoid -> next) -> Int -> String -> next
--
-- What happened during composition was that the @next@ in @fb1@ was used to insert
-- into @Int -> next@ the @String -> other_next@ from @fb2@; such that this results in
-- @Int -> (String -> other_next)@.
-- (Note: For clarity I renamed the type local type parameter @next@ to @other_next@ from @fb2@)
--
-- Also, there is the 'StaticContent' type class for types that have function builders.
--
-- NOTE: @FunctionBuilder w a b@ is actually @Cokleisli ((->) w) a b@
--       When @w@ is a 'Monoid' then, @((->) w)@ is a @Comonad@ instance.
--       The reason why this library does not use the comonad library under the hood
--       is currently only the missing 'Semigroup' and 'Monoid' instances; and
--       to avoid orphan instances a newtype wrapper would be required, and that
--       does not justify the additional dependency and the pollution of this library
--       with scary @Cokleislies@ and @Comonads@.
newtype FunctionBuilder acc next f_make_next = FB {runFunctionBuilder :: (acc -> next) -> f_make_next }

-- | Compose 'FunctionBuilder's such that the output function first takes all parameters
-- from the first 'FunctionBuilder' and then all parameters from the second 'FunctionBuilder' and then
-- appends the results of both functions, which is why we need the 'Monoid' constraint.
instance Monoid m => Category (FunctionBuilder m) where
  (.) (FB f) (FB g) = FB (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))
  id = FB ($ mempty)

-- | Allow appending a 'FunctionBuilder' to another without changing the resulting output function.
-- For example, 'FunctionBuilder's that have @FunctionBuilder m r r@ can append something to @m@.
-- It is not possible to add new parameters to the output function, this can only be done by
-- the 'Category' instance.
instance Semigroup m => Semigroup (FunctionBuilder m r r) where
  (<>) (FB f) (FB g) = FB (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))

-- | Allow appending a 'FunctionBuilder' to another without changing the resulting output function.
-- For example, 'FunctionBuilder's that have @FunctionBuilder m r r@ can append something to @m@.
-- It is not possible to add new parameters to the output function, this can only be done by
-- the 'Category' instance.
instance Monoid m => Monoid (FunctionBuilder m r r) where
  mappend = (<>)
  mempty  = id

instance Functor (FunctionBuilder m r) where
  fmap f (FB h) = FB (f . h)

instance Applicative (FunctionBuilder m r) where
  pure = FB . const
  (FB f) <*> (FB x) = FB (\k -> f k (x k))

instance Monad (FunctionBuilder m r) where
  return = pure
  (FB m) >>= f = FB (\k -> runFunctionBuilder (f (m k)) k)

-- | Get the composed __output function__ of a 'FunctionBuilder'.
--
-- The 'FunctionBuilder' passed to this function must match this signature:
--
-- > FunctionBuilder m m (arg0 -> .. -> m)
--
-- This means that the result of the generated function @arg0 -> .. -> m@ __MUST__ be
-- @m@, the underlying 'Monoid'.
--
-- The 'FunctionBuilder's generated by 'deferred' and 'immediate' are parametric
-- in the second type parameter and match the type signature required by this function.
--
-- Example 1:
--
-- > fb :: FunctionBuilder String String (Int -> Double -> Int -> String)
-- > fb = undefined
-- >
-- > example :: Int -> Double -> Int -> String
-- > example = toFunction  fb
--
-- Example 2:
--
-- > example :: Int -> Double -> Int -> String
-- > example = toFunction (i . d . i)
-- >
-- > s :: String -> FunctionBuilder String a a
-- > s x = FB (\k -> k x)
-- >
-- > i :: FunctionBuilder String next (Int -> next)
-- > i = FB (\k x -> k $ show x)
-- >
-- > d :: FunctionBuilder String next (Double -> next)
-- > d = FB (\k x -> k $ show x)
--
toFunction :: FunctionBuilder output output make_output -> make_output
toFunction = ($ id) . runFunctionBuilder

-- ** Building 'FunctionBuilder's

-- | A smart constructor for a 'FunctionBuilder' with that will build a function with
-- a parameter, and when the generated function is applied at that parameter
-- the function given here will be applied to the argument and the resulting monoidal
-- value will be appended to the result.
--
-- The generated builder can be passed to 'toFunction' since it is parametric
-- in its second type parameter.
--
-- Example:
--
-- When building a 'String' formatting 'FunctionBuilder'
-- the function to append a parameter that has a show instance could be:
--
-- > showing :: Show a => FunctionBuilder String r (a -> r)
-- > showing = deferred show
--
-- > example :: (Show a, Show b) => a -> b -> String
-- > example = toFunction (showing . showing)
--
-- >>> example True 0.33214
-- "True0.33214"
--
-- See the example in 'toFunction'.
--
-- @since 0.3.0.0
deferred :: (a -> m) -> FunctionBuilder m r (a -> r)
deferred f = FB (. f)

-- | Create a 'FunctionBuilder' that /appends/ something to the (monoidal-) output value.
--
-- This is a smart constructor for a 'FunctionBuilder'.
-- This functions is probably equal to:
--
-- > immediate x = FB (\k -> k x)
--
-- The generated builder can be passed to 'toFunction' since it is parametric
-- in its second type parameter.
--
-- Example:
--
-- When building a 'String' formatting 'FunctionBuilder'
-- the function to append a literal string could be:
--
-- > s :: String -> FunctionBuilder String a a
-- > s = immediate
--
-- > c :: Char -> FunctionBuilder String a a
-- > c = immediate . (:[])
--
-- > example :: String
-- > example = toFunction (s "hello" . c ' ' . s "world")
--
-- >>> example
-- "hello world"
--
-- See the example in 'toFunction'.
immediate :: m -> FunctionBuilder m r r
immediate m = FB ($ m)

-- ** Type Classes Creating 'FunctionBuilder'

-- | Types @a@ that can be turned into 'FunctionBuilder's
-- for a base monoid @m@.
--
-- This is the abstract version of 'StaticContent' and 'DynamicContent'
--
-- @since 0.1.2.0
class HasFunctionBuilder m a where
  -- | Get the function type (if any) of the builder.
  type ToFunction m a r
  type ToFunction m a r = r
  -- | Make a 'FunctionBuilder' from some value.
  toFunctionBuilder :: a -> FunctionBuilder m r (ToFunction m a r)

-- *** Specific Classes 'StaticContent' vs. 'DynamicContent'

-- | Types @a@ that can be turned into 'FunctionBuilder's
-- for a base monoid @m@.
--
-- These type can provide a function to work on the internal monoid,
--
-- They can be constructed using 'immediate'.
--
-- Of course they can incorporate information __statically known at compile time__
-- or via type class dictionaries (through singletons for instance).
--
-- For example:
--
-- > instance forall s . (KnownSymbol s) => StaticContent String (Proxy s) where
-- >   addStaticContent = immediate (symbolVal (Proxy @s))
--
--
-- @since 0.2.0.0
class StaticContent m a where
  -- | Return a 'FunctionBuilder' that can work on the underlying monoid.
  addStaticContent :: a -> FunctionBuilder m next next
  default addStaticContent :: (a ~ m) => a -> FunctionBuilder m next next
  addStaticContent = immediate

-- | Types that have a 'FunctionBuilder' with a runtime @parameter@
-- for a base monoid @m@.
--
-- For example:
-- If an instance adds an @Int@ parameter, it will define this family instance:
--
-- > instance DynamicContent String (Proxy "%i") Int where
-- >    addParameter _ = deferred
--
-- @since 0.2.0.0
class DynamicContent m a parameter | m a -> parameter where
  -- | Create a 'FunctionBuilder' that adds a parameter to the output function,
  -- and converts that argument to a value that can be accumulated in the
  -- resulting monoidal value.
  addParameter :: a -> FunctionBuilder m next (parameter -> next)
  default addParameter :: (a ~ (parameter -> m)) => a -> FunctionBuilder m next (parameter -> next)
  addParameter = deferred

-- ** Modifying Parameters of 'FunctionBuilder's

-- | Take away a function parameter added with 'addParameter' by /pre -/ applying it to some
-- value.
--
-- For example:
--
-- > intArg :: FunctionBuilder MyMonoid a (Int -> a)
-- > intArg = deferred undefined
-- >
-- > stringArg :: FunctionBuilder MyMonoid a (String -> a)
-- > stringArg = deferred undefined
-- >
-- > twoInt :: FunctionBuilder MyMonoid a (Int -> String -> a)
-- > twoInt = intArg . stringArg
-- >
-- > example :: FunctionBuilder MyMonoid a (String -> a)
-- > example = fillParameter twoInt 42
--
--
-- This is equivalent to:
--
-- @
--     fillParameter f x = f <*> pure x
-- @
--
fillParameter :: FunctionBuilder m r (a -> b) -> a -> FunctionBuilder m r b
fillParameter (FB !f) x = FB $ \k -> f k x

-- | Convert a 'FunctionBuilder' for a function @(a -> b)@ to @(Tagged tag a -> b)@.
tagParameter
  :: forall tag m r a b
   . FunctionBuilder m r (a -> b)
  -> FunctionBuilder m r (Tagged tag a -> b)
tagParameter = fmap (\f -> f . untag)

-- ** 'FunctionBuilder' Transformations

-- | Compose to 'FunctionBuilder's such that the second 'FunctionBuilder' may depend on the intermediate result
-- of the first. Similar to a monadic bind '>>=' but more flexible sind the underlying
-- 'Monoid' may change too, for example:
--
-- > intText :: FunctionBuilder Text next (Int -> next)
-- > intText = deferred undefined
-- >
-- > unpackB :: Text -> FunctionBuilder String next next
-- > unpackB = immediate . unpack
-- >
-- > intStr :: FunctionBuilder String next (Int -> next)
-- > intStr = intText `bind` unpackB
--
bind
  :: FunctionBuilder m g_next f_g_next
  -> (m -> FunctionBuilder n next g_next)
  -> FunctionBuilder n next f_g_next
bind mbc fm =
  FB $ \kna -> runFunctionBuilder mbc (($ kna) . runFunctionBuilder . fm)

-- | Convert the accumulated (usually monoidal-) value,
-- this allows to change the underlying accumlator type.
mapAccumulator :: (m -> n) -> FunctionBuilder m a b -> FunctionBuilder n a b
mapAccumulator into (FB f) = FB (\k -> f (k . into))

-- | Convert the output of a 'FunctionBuilder' value; since most
-- 'FunctionBuilder's are parameteric in @r@ they also have @r@ in a
-- in @a@, such that @a@ always either is @r@ or is a
-- function returning @r@ eventually.
--
-- In order to get from a 'FunctionBuilder' that can accept a continuation returning it an @r@
-- to a 'FunctionBuilder' that accepts continuations returning an @s@ instead, we need to
-- apply a function @s -> r@ to the return value of the continuation.
--
-- Note that a 'mapNext' will not only change the @r@ to an @s@ but
-- probably also the the @a@, when it is parametric, as in this contrived example:
--
-- > example :: Int -> x -> Sum Int
-- > example = toFunction (ign add)
-- >
-- > add :: FunctionBuilder (Sum Int) next (Int -> next)
-- > add = FB (\k x -> k $ Sum x)
-- >
-- > ign :: FunctionBuilder m (x -> r) a -> FunctionBuilder m r a
-- > ign = mapNext const
--
-- Here the extra parameter @x@ is /pushed down/ into the @a@ of the @add@ 'FunctionBuilder'.
mapNext :: (s -> r) -> FunctionBuilder m r a -> FunctionBuilder m s a
mapNext outof (FB f) = FB (\k -> f (outof . k))
