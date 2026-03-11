module Visualize.Data.Char.Block where

import Visualize.Utility (printGrid, )

import qualified Data.Char.Block as Block

import Control.Applicative (pure, )
import Data.Traversable (sequenceA, )


{-
I would like to have a table with a symmetry
with respect to the diagonal to the right lower corner.
Unfortunately this is not possible.
A symbol that is symmetric with respect to this diagonal
must be placed on the diagonal of the table.
This diagonal consists of 4 symbols,
but there are 8 symmetric symbols.
These symmetric symbols can be constructed
by conditionally merging three symmetric basis symbols,
namely 'left upper block', 'right lower block',
'left lower and right upper block'.
-}
visualize :: IO ()
visualize = do
   printGrid
      (\r0 r1 -> Block.filled $ Block.Block r0 r1)
      (sequenceA $ pure [False, True])
      (sequenceA $ pure [False, True])
