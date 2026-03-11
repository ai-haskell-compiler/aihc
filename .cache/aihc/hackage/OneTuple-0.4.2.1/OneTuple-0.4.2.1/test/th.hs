{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Tuple.Solo
import Data.Tuple.Solo.TH (tupE)

main :: IO ()
main = print $ MkSolo 'x' == $(tupE [[| 'x' |]])
