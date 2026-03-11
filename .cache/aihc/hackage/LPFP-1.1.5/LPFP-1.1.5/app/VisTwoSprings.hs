{-# OPTIONS -Wall #-}

import LPFP.SimpleVec ( R, zeroV )
import LPFP.Mechanics3D ( posVec, simulateVis, v3FromVec )
import LPFP.MultipleObjects ( MultiParticleState(..) )
import LPFP.MOExamples ( twoSpringsInitial, twoSpringsUpdate )
import Vis ( VisObject(..), Flavour(..), red, green, blue )

main :: IO ()
main = simulateVis 1 20 twoSpringsInitial twoSpringsVisObject twoSpringsUpdate

twoSpringsVisObject :: MultiParticleState -> VisObject R
twoSpringsVisObject (MPS sts)
    = let r0 = posVec (sts !! 0)
          r1 = posVec (sts !! 1)
          springsObj = Line Nothing [v3FromVec zeroV
                                    ,v3FromVec r0
                                    ,v3FromVec r1]  blue 
          objs = [Trans (v3FromVec r0) (Sphere 0.1 Solid red)
                 ,Trans (v3FromVec r1) (Sphere 0.1 Solid green)
                 ,springsObj
                 ]
          vpm = 1  -- Vis units per meter
      in Scale (vpm,vpm,vpm) $ VisObjects objs
