{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise8 where

import Data.Registry
import Test.Tutorial.Application
import Test.Tutorial.Exercise2

erasedRegistry :: Registry '[ERASED_TYPES] '[ERASED_TYPES]
erasedRegistry = eraseTypes registry

newErasedApp :: App
newErasedApp = make @App erasedRegistry

normalizedRegistry :: Registry _ _
normalizedRegistry = normalize registry
