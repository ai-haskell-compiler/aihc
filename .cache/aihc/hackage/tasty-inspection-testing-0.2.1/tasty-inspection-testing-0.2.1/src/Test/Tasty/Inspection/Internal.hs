-- |
-- Module:      Test.Tasty.Inspection.Internal
-- Copyright:   (c) 2017 Joachim Breitner, 2021 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  andrew.lelechenko@gmail.com
--

{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Inspection.Internal (CheckResult(..)) where

import Test.Tasty.Providers (IsTest(..), testPassed, testFailed)

data CheckResult
    = ResSuccess
    | ResSuccessWithMessage String
    | ResFailure String

instance IsTest CheckResult where
    run = const $ const . pure . \case
      ResSuccess -> testPassed ""
      ResSuccessWithMessage msg -> testPassed msg
      ResFailure msg -> testFailed msg
    testOptions = pure []
