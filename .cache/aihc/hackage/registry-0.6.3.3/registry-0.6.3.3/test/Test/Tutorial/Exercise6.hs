{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise6 where

import Data.Registry
import Protolude
import System.Directory (doesFileExist)
import Test.Tutorial.Application

newCheckedSecretReader :: MonadIO m => SecretReaderConfig -> Logger IO -> Tag "unchecked" (SecretReader IO) -> m (SecretReader IO)
newCheckedSecretReader (SecretReaderConfig path) logger uncheckedReader = liftIO $ do
  exists <- doesFileExist (toS path)
  unless exists fileDoesNotExist
  pure $ unTag uncheckedReader
  where
    fileDoesNotExist = error logger ("file does not exist at " <> path)

registryIO :: Registry _ _
registryIO =
  funTo @IO App
    <: funTo @IO newUserInput
    <: funTo @IO (newCheckedSecretReader @IO)
    <: funTo @IO (tag @"unchecked" newSecretReader)
    <: funTo @IO newRng
    <: funTo @IO newLogger
    <: funTo @IO newConsole
    <: valTo @IO (SecretReaderConfig "test/Test/Tutorial/secret.txt")

newAppIO :: IO App
newAppIO = make @(IO App) registryIO
