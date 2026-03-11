{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--  This module provides functions to make values
--  out of a registry. The general algorithm is the following
--
--   1. for a given value type search in the existing list of values
--      a value with the same type. If found return it
--
--   2. if not found search a function having the desired output type
--      if found, now try to recursively make all the input parameters.
--      Keep a stack of the current types trying to be built.
--
--   3. when trying to make an input parameter if the current input type
--      is already in the types trying to be built then there is a cycle.
--      Return an error in that case
--
--   4. when a value has been constructed place it on top of the existing value
--      list so that it can be reused by other functions
module Data.Registry.Make where

import Data.Dynamic
import Data.Registry.Internal.Make
import Data.Registry.Internal.Stack
import Data.Registry.Internal.Types
import Data.Registry.Registry
import Data.Registry.Solver
import qualified Data.Text as T
import Protolude as P hiding (Constructor)
import Type.Reflection
import qualified Prelude (error)

-- | Make an element of type a out of the registry
make :: forall a ins out. (Typeable a) => Registry ins out -> a
make registry =
  -- if the registry is an unchecked one, built with +:
  -- this may fail
  case makeEither registry of
    Right a -> a
    Left e -> Prelude.error (toS e)

-- | Make an element of type a out of the registry
--   and check statically that the element can be built
makeSafe :: forall a ins out. (Typeable a, Solvable ins out, Contains a out) => Registry ins out -> a
makeSafe = make

-- | Make an element of type a out of the registry, for a registry
--   which was possibly created with +:
makeEither :: forall a ins out. (Typeable a) => Registry ins out -> Either Text a
makeEither = makeEitherWithContext (Context [(someTypeRep (Proxy :: Proxy a), Nothing)])

-- * SPECIALIZED VALUES

-- | make for specialized values
makeSpecialized :: forall a b ins out. (Typeable a, Typeable b) => Registry ins out -> b
makeSpecialized registry =
  case makeSpecializedEither @a @b registry of
    Right a -> a
    Left e -> Prelude.error (toS e)

-- | make for specialized values
makeSpecializedPath :: forall path b ins out. (PathToTypeReps path, Typeable b) => Registry ins out -> b
makeSpecializedPath registry =
  case makeSpecializedPathEither @path @b registry of
    Right a -> a
    Left e -> Prelude.error (toS e)

-- | makeEither for specialized values, in case you are using an unchecked registry
makeSpecializedEither :: forall a b ins out. (Typeable a, Typeable b) => Registry ins out -> Either Text b
makeSpecializedEither = makeEitherWithContext (Context [(someTypeRep (Proxy :: Proxy a), Nothing), (someTypeRep (Proxy :: Proxy b), Nothing)])

-- | makeEither for specialized values along a path, in case you are using an unchecked registry
makeSpecializedPathEither :: forall path b ins out. (PathToTypeReps path, Typeable b) => Registry ins out -> Either Text b
makeSpecializedPathEither = makeEitherWithContext (Context ((,Nothing) <$> toList (someTypeReps (Proxy :: Proxy path))))

-- | This version of make only execute checks at runtime
--   this can speed-up compilation when writing tests or in ghci
makeEitherWithContext :: forall a ins out. (Typeable a) => Context -> Registry ins out -> Either Text a
makeEitherWithContext context registry = do
  let values = mempty
  let entries = _entries registry
  let specializations = _specializations registry
  let modifiers = _modifiers registry
  let targetType = someTypeRep (Proxy :: Proxy a)
  --  use the makeUntyped function to create an element of the target type from a list of values and functions
  --  the list of values is kept as some State so that newly created values can be added to the current state
  case runStackWithValues
    values
    (makeUntyped targetType context entries specializations modifiers) of
    Left e ->
      Left . showRegistry $
        "\nCould not create a "
          <> show targetType
          <> " out of the registry:"
          <> "\n\n"
          <> e
    Right Nothing ->
      Left . showRegistry $
        "\nCould not create a "
          <> show targetType
          <> " out of the registry"
    Right (Just result) ->
      maybe
        (Left $ "\nCould not cast the computed value to a " <> show targetType <> ". The value is of type: " <> show (valueDynTypeRep result))
        Right (fromDynamic (valueDyn result))
  where
    showRegistry message = do
      let r = show registry
      -- this allows the display of registries of no more than ~ 30 entries
      -- which should fit on a laptop screen
      if (length . T.lines $ r) <= 35
        then
          "\nThe registry is"
            <> "\n\n"
            <> r
            <> "=====================\n"
            <> message
            <> "\n\nYou can check the registry displayed above the ===== line to verify the current values and entries\n"
        else
          message
            <> "\n\n (the registry is not displayed because it is too large)"
