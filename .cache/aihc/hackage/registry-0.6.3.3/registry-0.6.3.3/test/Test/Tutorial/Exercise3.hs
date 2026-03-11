{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise3 where

import Data.Registry
import Protolude
import Test.Tutorial.Application
import Test.Tutorial.Exercise2

silentLogger :: Logger IO
silentLogger = Logger (const (pure ())) (const (pure ()))

silentRegistry = fun silentLogger <: registry

newSilentApp :: App
newSilentApp = make @App silentRegistry

newMisconfiguredApp :: App
newMisconfiguredApp = make @App (val (SecretReaderConfig "missing") <: registry)

newMisconfiguredSilentApp :: App
newMisconfiguredSilentApp = make @App (val (SecretReaderConfig "missing") <: silentRegistry)

newMisconfiguredRngSilentApp :: App
newMisconfiguredRngSilentApp =
  make @App $
    specialize @(Rng IO) (fun silentLogger) $
      val (SecretReaderConfig "missing") <: registry
