-- | \"Quick, gimme everything in the 'rando' package!\"

module Rando (
   -- We export the functions instead of the modules, so that all the docs are
   --   on one page:

   --   module System.Random.Pick
     pickOne
   , flipCoin

   -- , module System.Random.Shuffle.FisherYates
   , shuffle
   ) where

import System.Random.Pick
import System.Random.Shuffle.FisherYates

