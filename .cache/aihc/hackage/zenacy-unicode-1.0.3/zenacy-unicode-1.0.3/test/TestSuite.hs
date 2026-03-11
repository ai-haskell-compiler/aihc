module Main where

import Zenacy.Unicode.Tests
import Test.Framework
  ( defaultMain
  )

main :: IO ()
main = defaultMain
  [ testUnicode
  ]
