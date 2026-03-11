{-# OPTIONS -Wall #-}

import LPFP.SimpleVec ( vec )
import LPFP.Electricity ( elementaryCharge )
import LPFP.CoordinateSystems ( cart )
import LPFP.Charge ( protonOrigin )
import LPFP.ElectricField ( eField, epsilon0 )
import LPFP.Lorentz ( ParticleFieldState(..), animatePFS, defaultPFS )

main :: IO ()
main = animatePFS period 30 (4*bohrRadius)
       ( defaultPFS { mass          = electronMass
                    , charge        = -elementaryCharge  -- electron charge
                    , position      = cart bohrRadius 0 0
                    , velocity      = vec 0 v0 0
                    , electricField = eField protonOrigin } )
           where electronMass = 9.109e-31  -- kg
                 bohrRadius   = 0.529e-10  -- meters
                 v0 = elementaryCharge
                      / sqrt (4 * pi * epsilon0 * electronMass * bohrRadius)
                 period = 2 * pi * bohrRadius / v0
