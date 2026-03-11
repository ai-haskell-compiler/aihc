{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.RegistrySpec where

import Data.IORef
import Data.Registry
import Protolude as P
import Test.Tasty.Extensions

test_create_value_with_no_args_constructor = prop "no args constructors are considered as functions" $ do
  ref <- liftIO $ newIORef ("" :: Text)
  let registry' = funTo @IO refLogger +: funTo @IO ref +: registry

  Logger {..} <- liftIO $ make @(IO Logger) registry'
  liftIO $ info "hey"

  result <- liftIO $ readIORef ref
  result === "hey"

test_append_values = test "2 values can be appended together" $ do
  let r = val (1 :: Int) <: val (2 :: Int)
  make @Int r === 1

  let r2 = val (1 :: Int) <+ val (2 :: Int)
  make @Int r2 === 1

test_append_value_to_registry = test "a value can be appended to a registry" $ do
  let r = (val (1 :: Int) <: val (2 :: Int)) <: val (3 :: Int)
  make @Int r === 1

  let r2 = (val (1 :: Int) <+ val (2 :: Int)) <+ val (3 :: Int)
  make @Int r2 === 1

test_prepend_value_to_registry = test "a value can be prepended to a registry" $ do
  let r = val (1 :: Int) <: (val (2 :: Int) <: val (3 :: Int))
  make @Int r === 1

  let r2 = val (1 :: Int) <+ (val (2 :: Int) <+ val (3 :: Int))
  make @Int r2 === 1

-- *

newtype Logger = Logger {info :: Text -> IO ()}

newLogger :: IO Logger
newLogger = pure (Logger print)

refLogger :: IORef Text -> Logger
refLogger ref = Logger (writeIORef ref)

registry =
  fun newLogger
    +: end

-- * COMPILATION CHECK WITH THE <: operator

registry1 :: Registry '[] '[Int, Text]
registry1 =
  normalize $
    val (1 :: Int)
      <: (val ("t" :: Text) +: end)
      <: (val ("t" :: Text) +: end)
      <: val ("t" :: Text)

registry2 :: Registry '[] [Text, Text]
registry2 =
  val ("t" :: Text)
    <: val ("t" :: Text)

registry3 :: Registry '[] '[Int] =
  val (10 :: Int)
    <+ end

-- * COMPILATION CHECK LIFTING (see #7)

a :: Int -> Int -> IO Int
a _ _ = pure 0

b :: Int -> Int -> Rio Int
b = outTo @Rio liftIO a

c :: Rio Int -> Rio Int -> Rio Int
c = allTo @Rio b

-- here the result of outTo needs to be explicit
-- otherwise the type of d is Rio (Int -> Int -> Rio Int)
d :: Rio Int -> Rio Int -> Rio Int
d = allTo @Rio (outTo @Rio liftIO a :: Int -> Int -> Rio Int)

-- to avoid the issue with type inference above, we can use argsTo
e :: Rio Int -> Rio Int -> Rio Int
e = argsTo @Rio (outTo @Rio liftIO a)
