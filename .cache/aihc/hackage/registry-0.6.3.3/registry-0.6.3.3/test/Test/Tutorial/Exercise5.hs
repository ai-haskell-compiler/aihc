{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise5 where

import qualified Data.ByteString.Char8 as BS8
import Data.Registry
import Protolude
import System.Directory (doesFileExist)
import Test.Tutorial.Application

newCheckedSecretReader :: MonadIO m => SecretReaderConfig -> Logger IO -> m (SecretReader IO)
newCheckedSecretReader (SecretReaderConfig path) logger = liftIO $ do
  exists <- doesFileExist (toS path)
  unless exists fileDoesNotExist
  pure
    SecretReader
      { readSecret =
          if exists
            then Just . decodeUtf8 <$> BS8.readFile (toS path)
            else fileDoesNotExist $> Nothing
      }
  where
    fileDoesNotExist = error logger ("file does not exist at " <> path)

registryIO :: Registry _ _
registryIO =
  funTo @IO App
    <: funTo @IO newUserInput
    <: funTo @IO newRng
    <: funTo @IO (newCheckedSecretReader @IO)
    <: funTo @IO newLogger
    <: funTo @IO newConsole
    <: valTo @IO (SecretReaderConfig "test/Test/Tutorial/secret.txt")

newAppIO :: IO App
newAppIO = make @(IO App) registryIO
