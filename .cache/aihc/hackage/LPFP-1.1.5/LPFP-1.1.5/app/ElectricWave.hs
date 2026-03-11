{-# OPTIONS -Wall #-}

import LPFP.Maxwell ( makeEpng, stateUpdate, jGaussian, initialStateFDTD )
import Diagrams.Prelude ( black, yellow )

main :: IO ()
main = let dt = 0.02e-9   -- 0.02 ns time step
           numTimeSteps = 719
       in sequence_ $ map (makeEpng (yellow,black)) $ zip [0..numTimeSteps] $
          iterate (stateUpdate dt jGaussian) (initialStateFDTD 0.108)
