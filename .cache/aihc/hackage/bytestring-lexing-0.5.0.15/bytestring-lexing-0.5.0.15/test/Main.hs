{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  test/Main
-- Copyright   :  Copyright (c) 2015--2021 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@cpan.org
-- Stability   :  benchmark
-- Portability :  Haskell98
--
-- Run all the basic correctness tests.
----------------------------------------------------------------
module Main (main) where
import qualified Test.Tasty as Tasty
import qualified Integral
import qualified Fractional

----------------------------------------------------------------
----------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain . Tasty.testGroup "Main" $
    [ Integral.tests
    , Fractional.tests
    ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
