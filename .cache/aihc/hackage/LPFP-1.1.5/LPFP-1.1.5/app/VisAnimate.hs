{-# OPTIONS -Wall #-}

import Vis

rotatingCube :: Float -> VisObject Float
rotatingCube t = RotEulerRad (Euler 0 0 t) (Cube 1 Solid blue)

orient :: VisObject Float -> VisObject Float
orient pict = RotEulerDeg (Euler 270 180 0) $ pict

main :: IO ()
main = animate defaultOpts (orient . rotatingCube)
