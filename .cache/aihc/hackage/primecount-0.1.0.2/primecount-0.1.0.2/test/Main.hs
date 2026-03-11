-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD-3-Clause
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
module Main (main) where

import qualified Test.Math.NumberTheory.Prime.Count (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "" [Test.Math.NumberTheory.Prime.Count.tests]
