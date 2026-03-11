{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.RegistrySpec where

import Data.Dynamic
import Data.Registry.Internal.Registry
import Data.Registry.Internal.Stack
import Data.Registry.Internal.Types
import Protolude as P hiding (show)
import Test.Data.Registry.Internal.Gens
import Test.Data.Registry.Internal.GensRegistry
import Test.Tasty.Extensions

test_find_no_value = prop "no value can be found if nothing is stored in the registry" $ do
  value <- forAll $ gen @Int

  (fromValueDyn <$> findValueOrSpecialization (valueDynTypeRep (createValue value)) mempty mempty mempty) === (Nothing :: Maybe (Maybe Int))

test_find_value = prop "find a value in a list of values when there are no specializations" $ do
  (value, values) <- forAll genValues

  (fromValueDyn <$> findValueOrSpecialization (valueDynTypeRep (createValue value)) mempty mempty values) === Just (Just value)

test_find_specialized_value = prop "find a value in a list of values when there is a specialization for a given context" $ do
  value <- forAll $ gen @Int
  values <- forAll $ gen @Values
  let listTypeRep = dynTypeRep . toDyn $ [value]
  let context = Context [(listTypeRep, Nothing)] -- when trying to build a [Int]
  let specializations = Specializations [Specialization (pure listTypeRep) (UntypedValue $ createValue value)]

  (fromValueDyn <$> findValueOrSpecialization (valueDynTypeRep (createValue value)) context specializations values) === Just (Just value)

test_find_no_constructor = prop "no constructor can be found if nothing is stored in the registry" $ do
  value <- forAll $ gen @Int

  (fromDynamic . untypedDyn <$> findUntyped (valueDynTypeRep (createValue value)) mempty) === (Nothing :: Maybe (Maybe Int))

test_find_contructor = prop "find a constructor in a list of constructors" $ do
  (TextToInt function) <- forAll $ gen @TextToInt
  functions <- forAll $ addEntry (TypedFunction $ createFunction function)  <$> gen @Entries

  let outputType = dynTypeRep (toDyn (1 :: Int))

  (fmap TextToInt <$> (fromDynamic . untypedDyn <$> findUntyped outputType functions))
    === Just (Just (TextToInt function))

test_store_value_no_modifiers = prop "a value can be stored in the list of values" $ do
  (value, values) <- forAll genValues

  let createdValue = createValue value
  let (Right stored) = execStackWithValues values (storeValue mempty createdValue)

  (fromValueDyn <$> findValueOrSpecialization (dynTypeRep . toDyn $ value) mempty mempty stored) === Just (Just value)


test_store_value_with_modifiers = prop "a value can be stored in the list of values but modified beforehand" $ do
  (value, values) <- forAll genValues

  let valueType = dynTypeRep . toDyn $ value
  let modifiers = Modifiers [(valueType, createConstModifierFunction (\(i :: Int) -> i + 1))]
  let createdValue = createValue value
  let (Right stored) = execStackWithValues values (storeValue modifiers createdValue)

  (fromValueDyn <$> findValueOrSpecialization valueType mempty mempty stored) === Just (Just (value + 1))

test_store_value_ordered_modifiers = prop "modifiers are applied in a LIFO order" $ do
  (value, values) <- forAll genValues

  let valueType = dynTypeRep . toDyn $ value
  let modifiers =
        Modifiers
          [ (valueType, createConstModifierFunction (\(i :: Int) -> i * 2)),
            (valueType, createConstModifierFunction (\(i :: Int) -> i + 1))
          ]
  let createdValue = createValue value
  let (Right stored) = execStackWithValues values (storeValue modifiers createdValue)

  (fromValueDyn <$> findValueOrSpecialization valueType mempty mempty stored) === Just (Just ((value * 2) + 1))

-- *

fromValueDyn (Right value) = fromDynamic . valueDyn $ value
fromValueDyn (Left specialization) =
  case createValueFromSpecialization mempty specialization of
    UntypedValue v -> fromDynamic . valueDyn $ v
    UntypedFunction _ -> panic "expected to find a specialized value"
