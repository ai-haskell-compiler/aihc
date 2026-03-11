{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | This module provides a version of "GHC.Records" as it will be after the implementation of
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0042-record-set-field.rst GHC proposal #42>.

  In future GHC versions it will be an alias for "GHC.Records".
-}
module GHC.Records.Compat (
    HasField (..),
    getField,
    setField,
) where

{- | Constraint representing the fact that the field @x@ can be get and set on
  the record type @r@ and has field type @a@.  This constraint will be solved
  automatically, but manual instances may be provided as well.

  The function should satisfy the invariant:

> uncurry ($) (hasField @x r) == r
-}
class HasField x r a | x r -> a where
    -- | Function to get and set a field in a record.
    hasField :: r -> (a -> r, a)

-- | Get a field in a record.
getField :: forall x r a. (HasField x r a) => r -> a
getField = snd . hasField @x

-- | Set a field in a record.
setField :: forall x r a. (HasField x r a) => r -> a -> r
setField = fst . hasField @x
