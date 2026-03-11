{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Make.SpecializationFunctionsSpec where

import Data.Registry
import Protolude hiding (C1)
import Test.Tasty.Extensions

-- | Case 1: contextual setting of different values for a given type
test_specialization_1 = test "values can be built from specialized functions depending on some context" $ do
  -- this function uses the default Int (and has unused parameters)
  let useDefaultInt (_t :: Text) = Config
  -- this function doubles the default Int (and has unused parameters)
  let useTwiceDefaultInt i (_t :: Text) (_b :: Bool) = Config $ i * 2

  (c1, c2) <- liftIO $ do
    let r =
          specialize @UseConfig1 (fun useDefaultInt)
            . specialize @UseConfig2 (fun useTwiceDefaultInt)
            $ fun newUseConfig2
              <: fun newUseConfig1
              <: val (Config 3)
              <: val (1 :: Int)
              <: val True
              <: val ("text" :: Text)

    pure (printConfig1 (make @UseConfig1 r), printConfig2 (make @UseConfig2 r))

  c1 === Config 1
  c2 === Config 2

test_missing_inputs = test "values can be built from specialized functions depending on some context" $ do
  -- this function uses the default Int (and has unused parameters)
  let useDefaultInt (_t :: Text) = Config
  -- this function doubles the default Int (it has unused parameters, but will be missing the Bool parameter which is not in the registry)
  let useTwiceDefaultInt i (_t :: Text) (_b :: Bool) = Config $ i * 2

  (c1, c2) <- liftIO $ do
    let r =
          specialize @UseConfig1 (fun useDefaultInt)
            . specialize @UseConfig2 (fun useTwiceDefaultInt)
            $ fun newUseConfig2
              <: fun newUseConfig1
              <: val (Config 3)
              <: val (1 :: Int)
              <: val ("text" :: Text)

    pure (printConfig1 (make @UseConfig1 r), printConfig2 (make @UseConfig2 r))

  c1 === Config 1

  annotate "if inputs are missing for a specialization, we use the default value"
  c2 === Config 3

test_override_bug = test "we can override an IO value with an IO function" $ do
  let r = funTo @IO (Config 2)
            <: valTo @IO (Config 1)

  c1 <- liftIO $ make @(IO Config) r

  c1 === Config 2

test_override_specialized_bug = test "we can specialize an IO value with an IO function" $ do
  let r = specialize @(IO UseConfig2) (funTo @IO $ \(n::Int) -> Config n) $
         funTo @IO UseConfig2
        <: funTo @IO UseConfig1
        <: funTo @IO (Config 2)
            <: valTo @IO (Config 1)
            <: valTo @IO (5 :: Int)

  uc1 <- liftIO $ make @(IO UseConfig1) r
  uc2 <- liftIO $ make @(IO UseConfig2) r
  printConfig1 uc1 === Config 2
  printConfig2 uc2 === Config 5


-- we want the following graph
{-
            +----------  Base  ------------+
            |                              |
            v                              v
   (client1 :: Client1)          (client2 :: Client2)
            |                              |
            v                              v
   (useConfig1 :: UseConfig) (useConfig2 :: UseConfig)
            |                              |
            v                              v
   (config1 :: Config)           (config2 :: Config)

-}

newtype Config = Config {configInt :: Int} deriving (Eq, Show)

newtype UseConfig1 = UseConfig1 {printConfig1 :: Config}

newUseConfig1 config = UseConfig1 {printConfig1 = config}

newtype UseConfig2 = UseConfig2 {printConfig2 :: Config}

newUseConfig2 config = UseConfig2 {printConfig2 = config}

newtype UseConfig = UseConfig {printConfig :: Config}

newUseConfig config = UseConfig {printConfig = config}

newtype Client1 = Client1 {printClientConfig1 :: Config}

newClient1 useConfig = Client1 {printClientConfig1 = printConfig useConfig}

newtype Client2 = Client2 {printClientConfig2 :: Config}

newClient2 useConfig = Client2 {printClientConfig2 = printConfig useConfig}

newtype Base = Base {printBase :: (Config, Config)}

newBase client1 client2 = Base {printBase = (printClientConfig1 client1, printClientConfig2 client2)}
