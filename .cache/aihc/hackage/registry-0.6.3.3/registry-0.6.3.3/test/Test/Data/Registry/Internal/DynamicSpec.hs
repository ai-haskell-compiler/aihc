{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.DynamicSpec where

import Data.Dynamic
import Data.Registry.Internal.Dynamic
import Data.Registry.Internal.Types
import Data.Text as T
import Protolude as P
import Test.Tasty.Extensions
import Type.Reflection hiding (typeRep)

test_collectInputTypes = test "we can collect the input types of a function" $ do
  collectInputTypes (createFunction (u :: Int)) === []
  collectInputTypes (createFunction (u :: Text -> Int)) === [dynType (u :: Text)]
  collectInputTypes (createFunction (u :: Int -> Text -> Int)) === [dynType (u :: Int), dynType (u :: Text)]
  collectInputTypes (createFunction (u :: Int -> Maybe Double -> Maybe Text)) === [dynType (u :: Int), dynType (u :: Maybe Double)]

test_outputType = test "we can get the output type of a function" $ do
  outputType (dynType (u :: Int)) === dynType (u :: Int)
  outputType (dynType (u :: Text -> Int)) === dynType (u :: Int)
  outputType (dynType (u :: Int -> Text -> Int)) === dynType (u :: Int)
  outputType (dynType (u :: Int -> Maybe Double -> Maybe Text)) === dynType (u :: Maybe Text)

test_applyFunction = test "we can apply a list of dynamic values to a dynamic function" $ do
  (fromDynamic @Int . valueDyn <$> applyFunction (createFunction T.length) [createValue ("hello" :: Text)]) === Right (Just 5)

  let add1 (i :: Int) (j :: Int) = P.show (i + j) :: Text
  (fromDynamic @Text . valueDyn <$> applyFunction (createFunction add1) [createValue (1 :: Int), createValue (2 :: Int)]) === Right (Just "3")

  -- no value is returned when an input parameter is incorrect
  (fromDynamic @Int . valueDyn <$> applyFunction (createFunction T.length) [createValue (1 :: Int)]) === Left "failed to apply <<Int>> to : <<Text -> Int>>"

  -- no value is returned when there are not enough inputs
  (fromDynamic @Text . valueDyn <$> applyFunction (createFunction add1) []) === Left "the function Int -> Int -> Text cannot be applied to an empty list of parameters"

u = undefined

dynType :: forall a. (Typeable a) => a -> SomeTypeRep
dynType = dynTypeRep . toDyn
