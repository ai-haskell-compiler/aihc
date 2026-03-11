module Main where

import qualified Test.Data.Char.Block as Block
import qualified Test.Data.Char.Frame as Frame

main :: IO ()
main = do
   Block.test
   Frame.test
