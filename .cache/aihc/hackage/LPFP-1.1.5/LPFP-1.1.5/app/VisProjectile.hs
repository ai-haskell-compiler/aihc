{-# OPTIONS -Wall #-}

import LPFP.SimpleVec ( R, (*^) )
import LPFP.Mechanics3D
    ( ParticleState(..), simulateVis
    , projectileInitial, projectileUpdate, v3FromVec )
import Vis
    ( VisObject(..), Flavour(..), red )
import System.Environment
    ( getArgs )

projectileVisObject :: ParticleState -> VisObject R
projectileVisObject st
    = let r = posVec st
      in Trans (v3FromVec (0.01 *^ r)) (Sphere 0.1 Solid red)

mainWithArgs :: [String] -> IO ()
mainWithArgs args
    = simulateVis 3 20
      (projectileInitial args) projectileVisObject projectileUpdate

main :: IO ()
main = getArgs >>= mainWithArgs
