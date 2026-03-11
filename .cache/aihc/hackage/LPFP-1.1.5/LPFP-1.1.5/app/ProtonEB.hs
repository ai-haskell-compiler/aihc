{-# OPTIONS -Wall #-}

import LPFP.SimpleVec ( vec )
import LPFP.Electricity ( elementaryCharge )
import LPFP.Lorentz ( ParticleFieldState(..), animatePFS, defaultPFS )

main :: IO ()
main = animatePFS 1e-5 30 0.05
       ( defaultPFS { mass          = 1.673e-27  -- proton in kg
                    , charge        = elementaryCharge
                    , velocity      = vec 0 2000 0
                    , electricField = \_ -> vec 0 20 0
                    , magneticField = \_ -> vec 0  0 0.01 } )
