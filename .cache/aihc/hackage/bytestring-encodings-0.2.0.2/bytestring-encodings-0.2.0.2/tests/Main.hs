module Main where

import qualified Ascii as Asc
import qualified Utf8  as Utf8

main :: IO Bool
main = do 
  Asc.testAscii
  Utf8.testUtf8
