{- Tests for the HUnit-approx package

   Copyright (c) 2014 Richard Eisenberg
   rae@cs.brynmawr.edu

-}

{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Test.HUnit
import Test.HUnit.Approx
import Control.Monad
import Control.Exception

main :: IO ()
main = void $ runTestTT tests

assertNot :: Assertion -> Assertion
assertNot asrt = do
  result <- try asrt
  case result of
    Left (_ :: SomeException) -> return () -- success!
    Right _                   -> assertFailure "Inverted assertion succeeded"

tests :: Test
tests =
  let ?epsilon = 0.0001 in
  TestList [ "refl" ~: 4.0 ~?~ 4.0
           , "manual" ~: 4.0 ~~? 4.000000001
           , "calc" ~: (0.1 + 0.1 + 0.1 @?~ 0.3)
           , "not" ~: assertNot (4.005 @~? 4) ]
