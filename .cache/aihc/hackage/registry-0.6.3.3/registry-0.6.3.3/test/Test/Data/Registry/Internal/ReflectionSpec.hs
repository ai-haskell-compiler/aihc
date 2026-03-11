{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.ReflectionSpec where

import Data.Dynamic
import Data.Registry.Internal.Reflection
import Data.Registry.Internal.Types
import Protolude as P
import Test.Tasty.Extensions
import Prelude (String)

test_is_function = test "we can check if a type rep represents a function" $ do
  isFunction (dynTypeRep $ toDyn (u :: Int -> Int)) === True
  isFunction (dynTypeRep $ toDyn (u :: Int -> IO Int)) === True
  isFunction (dynTypeRep $ toDyn (u :: Int)) === False
  isFunction (dynTypeRep $ toDyn (u :: IO Int)) === False

test_show_value = test "show value for a simple type" $ do
  valDescriptionToText (describeValue (1 :: Int)) === "Int: 1"
  valDescriptionToText (describeValue (1 :: Double)) === "Double: 1.0"
  valDescriptionToText (describeValue (True :: Bool)) === "Bool: True"
  valDescriptionToText (describeValue ("1" :: Text)) === "Text: \"1\""
  valDescriptionToText (describeValue ("1" :: String)) === "String: \"1\""

test_show_value_nested_type = test "show value for a nested types" $ do
  valDescriptionToText (describeValue (Just 1 :: Maybe Int)) === "Maybe Int: Just 1"

  -- putting parentheses around types doesn't really work when type constructors
  -- have more than one argument :-(
  valDescriptionToText (describeValue (Right 1 :: Either Text Int)) === "Either (Text Int): Right 1"
  valDescriptionToText (describeValue ([1] :: [Int])) === "[Int]: [1]"

  -- user types must be shown with their full component names
  valDescriptionToText (describeValue mod1) === "Test.Data.Registry.Internal.ReflectionSpec.Mod Int: Mod 1 \"hey\""

test_show_function = test "show simple functions" $ do
  funDescriptionToText (describeFunction add1) === "Int -> Int"
  funDescriptionToText (describeFunction add2) === "Int -> Int -> Text"
  funDescriptionToText (describeFunction iomod) === "IO (Test.Data.Registry.Internal.ReflectionSpec.Mod Int)"

  funDescriptionToText (describeFunction fun0) === "IO Int"
  funDescriptionToText (describeFunction fun1) === "IO Int -> IO Int"
  funDescriptionToText (describeFunction fun2) === "IO Int -> IO Int -> IO Int"
  funDescriptionToText (describeFunction fun3) === "IO (Test.Data.Registry.Internal.ReflectionSpec.Mod Int) -> IO Int"

-- * helpers

u = undefined

data Mod a = Mod a Text deriving (Eq, Show)

mod1 :: Mod Int
mod1 = Mod 1 "hey"

iomod :: IO (Mod Int)
iomod = pure (Mod 1 "hey")

add1 :: Int -> Int
add1 i = i + 1

add2 :: Int -> Int -> Text
add2 _ = undefined

fun0 :: IO Int
fun0 = undefined

fun1 :: IO Int -> IO Int
fun1 = undefined

fun2 :: IO Int -> IO Int -> IO Int
fun2 = undefined

fun3 :: IO (Mod Int) -> IO Int
fun3 = undefined
