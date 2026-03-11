module Main where

import qualified Visualize.Data.Char.Block as Block
import qualified Visualize.Data.Char.Frame as Frame
import qualified Visualize.Data.Char.Number as Number
import qualified Visualize.Data.Char.Small as Small


main :: IO ()
main = do
   Block.visualize
   Frame.visualize
   Number.visualize
   Small.visualize
