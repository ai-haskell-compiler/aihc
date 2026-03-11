module Visualize.Data.Char.Frame where

import Visualize.Utility (printGrid, )

import qualified Data.Char.Frame as Frame

import Control.Applicative (pure, liftA2, )
import Data.Traversable (sequenceA, )


visualize :: IO ()
visualize = do
   printGrid
      (\v h -> Frame.simple $ Frame.Parts v h)
      (sequenceA $ pure [False, True])
      (sequenceA $ pure [False, True])

   printGrid
      (\(dv,v) (dh,h) ->
          maybe '?' id $
          Frame.doubleMaybe (Frame.Directions dv dh) (Frame.Parts v h))
      (liftA2 (,)
          [False, True] (sequenceA $ pure [minBound .. maxBound]))
      (liftA2 (,)
          [False, True] (sequenceA $ pure [minBound .. maxBound]))

   printGrid
      (\v h -> Frame.weighted $ Frame.Parts v h)
      (sequenceA $ pure [minBound .. maxBound])
      (sequenceA $ pure [minBound .. maxBound])
