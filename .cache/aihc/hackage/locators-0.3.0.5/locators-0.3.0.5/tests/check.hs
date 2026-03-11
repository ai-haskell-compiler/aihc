--
-- Human exchangable identifiers and locators
--
-- Copyright © 2013-2018 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

module Main where

import Test.Hspec (hspec)

import TestSuite (suite)

main :: IO ()
main = do
    hspec suite
    putStrLn "Ok"
