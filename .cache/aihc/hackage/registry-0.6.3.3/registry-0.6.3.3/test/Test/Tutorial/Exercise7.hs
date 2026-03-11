{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise7 where

import Data.Registry
import Protolude
import Test.Tutorial.Application

-- | This makes sure that there is only one logger ever used in the application
newCachedLogger :: Rio (Logger IO)
newCachedLogger = singleton $ do
  print ("start the logger" :: Text)
  pure (Logger putStrLn putStrLn)

registry :: Registry _ _
registry =
  funTo @Rio App
    <: funTo @Rio newUserInput
    <: funTo @Rio newRng
    <: funTo @Rio newSecretReader
    <: fun newCachedLogger
    <: funTo @Rio newConsole
    <: valTo @Rio (SecretReaderConfig "test/Test/Tutorial/secret.txt")

newAppIO :: IO App
newAppIO = withRegistry @App registry pure
