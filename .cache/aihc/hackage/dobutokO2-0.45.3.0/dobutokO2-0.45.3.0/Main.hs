-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A program and a library to create experimental music
-- from a mono audio and a Ukrainian text.

module Main where

import DobutokO.Sound.Executable (dobutokO2)


{-- Main function.
--}
main :: IO ()
main = dobutokO2
