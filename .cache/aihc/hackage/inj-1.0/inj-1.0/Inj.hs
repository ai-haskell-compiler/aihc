{- |

An injection is a function that never maps distinct elements of the domain to
the same element of the codomain. For example, @(\\x -> x + 1)@ is an injection,
but @(\\x -> min x 0)@ is not.

Injections can be used to construct nested structures from singleton elements.

-}

{-# LANGUAGE NoImplicitPrelude,
             DefaultSignatures,
             MultiParamTypeClasses,
             TypeFamilies #-}

module Inj (Inj(..)) where

-- | Inject @p@ into @a@.
--
-- By convention, the instances of @Inj@ never match on @p@ and always match on
-- @a@. This guarantees that the users will not encounter overlapping instances.
class Inj p a where
  -- | Inject @p@ into @a@.
  inj :: p -> a

  default inj :: (p ~ a) => p -> a
  inj = \x -> x

-- @instance Inj a a@ is tempting to define. Unfortunately, it does not work
-- as well as one might hope. For instance, consider a type like this:
--
-- @
-- data Shape x = Circle | Rectangle | Other x
--   deriving Functor
-- @
--
-- If we want to write @inj Circle@, then we get an ambiguity error:
--
-- @
--    * Could not deduce (Inj (Shape x0) (Shape x))
--        arising from a use of `inj'
-- @
--
-- That is because @Inj a a@ for @Shape x@ is equivalent to
--
-- @instance Inj (Shape x) (Shape x)@
--
-- but for good type inference we want
--
-- @instance (x1 ~ x2) => Inj (Shape x1) (Shape x2)@
--
-- Furthermore, we can take advantage of @Shape@ being a functor and
-- define an even better instance:
--
-- @
-- instance Inj a b => Inj (Shape a) (Shape b) where
--   inj = fmap inj
-- @
--
-- Unfortunately, both of the better instances are overlapping with @Inj a a@.
