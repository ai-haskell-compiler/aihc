module Test.Data.Char.Block where

import Test.Utility (checkDuplicates, )

import qualified Data.Char.Block as Block

import Control.Applicative (pure, )
import Data.Traversable (sequenceA, )


test :: IO ()
test = do
   putStrLn "Check duplicates of block graphics"
   checkDuplicates (sequenceA $ pure [False, True]) Block.filled
