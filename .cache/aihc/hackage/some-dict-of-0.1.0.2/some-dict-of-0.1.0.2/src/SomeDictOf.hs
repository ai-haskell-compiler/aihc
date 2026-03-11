{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SomeDictOf
    ( module SomeDictOf
    -- * Re-exports
    , module Data.Functor.Identity
    , module Data.Proxy
    , module Data.Constraint
    ) where

import Data.Constraint
import Data.Functor.Identity
import Data.Kind
import Data.Proxy

-- | A datatype that carries evidence of type class membership for some
-- type, along with a datatype indexed by that type. This is confusing, so
-- let's look at some examples.
--
-- The alias @'SomeDict' clazz@ uses 'Proxy' for the datatype index. This means
-- that the wrapper is a @'Proxy' :: 'Proxy' a@, and you know that the type
-- @a@ has an instance of @clazz a@. The 'SomeDictOf' type does not actually
-- *carry* any values of this type.
--
-- That is not to say that we *necessarily* won't have a value - consider
-- @'SomeDictOf' 'Identity'@, which actually does hold a value. We can use this
-- to make an existential 'Show' wrapper.
--
-- @
-- showSomeDict :: 'SomeDictOf' 'Identity' 'Show'
-- showSomeDict =
--     'SomeDictOf' ('Identity' 3 :: 'Identity' 'Int')
-- @
--
-- We can happily have a @['SomeDictOf' 'Identity' 'Show']@, or a @Map String
-- ('SomeDictOf' 'Identity' 'Show)@, or similar.
--
-- Constructing them is easy enough. Consuming them can be a bit trickier. Let's
-- look at a @'SomeDict' 'Monoid'@.
--
-- @
-- monoid :: 'SomeDictOf' 'Proxy' 'Monoid'
-- monoid =
--     'SomeDictOf' ('Proxy' :: 'Proxy' 'Data.Text.Text')
-- @
--
-- All we know about this is that it's carrying a datatype with a 'Monoid'
-- instance. We'll case-match on the value, and then in the case branch, we'll
-- have evidence that (whatever the underlying type is) that it has a 'Monoid'
-- instance.
--
-- We'll repackage the value, but instead of using 'Proxy', we'll stuff 'mempty'
-- into 'Identity'.
--
-- @
-- useMonoid :: 'SomeDictOf' 'Proxy' 'Monoid' -> 'SomeDictOf' 'Identity' 'Monoid'
-- useMonoid someDictOfProxy =
--     case someDictOfProxy of
--         SomeDictOf (Proxy :: Proxy a) ->
--             SomeDictOf (Identity (mempty :: a))
-- @
--
-- @since 0.1.0.0
data SomeDictOf (f :: k -> Type) (c :: k -> Constraint) where
    SomeDictOf :: c a => f a -> SomeDictOf f c

-- | In the case where you only need evidence that the dictionary exists,
-- then you can use this type. By carrying a 'Proxy' instead of a real
-- value, we can summon these up whenever we have an instance of the type in
-- question.
--
-- This is useful when you have a type class that specifies some *static*
-- behavior that is useful, or a type class that can provide means of
-- creating/retrieving/working with data. Consider the @PersistEntity@ class
-- from the @persistent@ database library. An instance of that class can be used
-- to get the @EntityDef@ that describes how the type interacts with the
-- database, or you can even @selectList@ and grab all entities out of the
-- database.
--
-- @
-- someEntity :: 'SomeDict' PersistEntity
-- someEntity = someDict \@User
-- @
--
-- With this value, we can now extract the @EntityDef@:
--
-- @
-- someEntityDef :: EntityDef
-- someEntityDef =
--     case someEntity of
--         SomeDictOf (Proxy :: Proxy a) ->
--             entityDef (Proxy :: Proxy a)
-- @
--
-- We can also load all the rows out of the database.
--
-- @
-- verifyRowsCanLoad :: SqlPersistT IO ()
-- verifyRowsCanLoad =
--     case someEntity of
--         SomeDictOf (Proxy :: Proxy a) -> do
--             rows <- selectList [] [] :: SqlPersistT IO [Entity a]
--             mapM_ (print . entityKey) rows
-- @
--
-- @since 0.1.0.0
type SomeDict = SomeDictOf Proxy

-- | Construct a 'SomeDict' with the type in question. Use with
-- @TypeApplications@.
--
-- Example:
--
-- @
-- showDict :: SomeDict Show
-- showDict = someDict \@Int
--
-- entityDict :: SomeDict PersistEntity
-- entityDict = someDict \@User
-- @
--
-- @since 0.1.0.0
someDict :: forall a c. c a => SomeDict c
someDict = SomeDictOf (Proxy :: Proxy a)

-- | Construct a 'SomeDict' based on a 'Dict' from the "Data.Constraint" module.
--
-- @since 0.1.0.0
fromDict :: forall a c. Dict (c a) -> SomeDict c
fromDict Dict = SomeDictOf (Proxy @a)

-- | Unpack a 'SomeDictOf' and attain access to the evidence that the
-- underlying type satisfies the instance in question.
--
-- @since 0.1.0.0
withSomeDictOf
    :: SomeDictOf f c
    -> (forall a. c a => f a -> r)
    -> r
withSomeDictOf (SomeDictOf p) k =
    k p

-- | Transform the type index used in the 'SomeDictOf'. The provided function is
-- unable to inspect the type, but you will have the class operations available
-- to it.
--
-- Example:
--
-- @
-- provideMempty ;: SomeDictOf Proxy Monoid -> SomeDictOf Identity Monoid
-- provideMempty = mapSomeDictOf (\\proxy -> Identity mempty)
-- @
--
-- @since 0.1.0.0
mapSomeDictOf
    :: (forall a. c a => f a -> g a)
    -> SomeDictOf f c
    -> SomeDictOf g c
mapSomeDictOf f (SomeDictOf fa) =
    SomeDictOf (f fa)

-- |  Not really sure what this might be useful for.
--
-- Examples:
--
-- @
-- shows :: SomeDictOf [] Show
-- shows = SomeDictOf [True, False, True]
--
-- forgotten :: [SomeDictOf Proxy Show]
-- forgotten = forgetContents show
--
-- main = do
--     forM forgotten $ \\(SomeDictOf Proxy) -> do
--          print 10
-- @
--
-- The above program should output 10 three times.
--
-- @since 0.1.0.0
forgetContents
    :: forall f c. Functor f
    => SomeDictOf f c
    -> f (SomeDictOf Proxy c)
forgetContents (SomeDictOf (xs :: f a)) =
    fmap (\(_ :: a) -> SomeDictOf (Proxy @a)) xs
