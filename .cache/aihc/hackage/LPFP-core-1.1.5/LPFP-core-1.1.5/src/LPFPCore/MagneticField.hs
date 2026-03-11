{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFPCore.MagneticField
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 27 of the book Learn Physics with Functional Programming
-}

module LPFPCore.MagneticField where

import LPFPCore.SimpleVec ( Vec(..), R
                 , (^-^), (*^), (^/), (<.>), (><)
                 , magnitude )
import LPFPCore.CoordinateSystems
    ( VectorField
    , rVF, displacement, addVectorFields )
import LPFPCore.Geometry ( Curve(..), Surface(..), Volume(..) )
import LPFPCore.ElectricField
    ( curveSample, surfaceSample, volumeSample
    , vectorSurfaceIntegral, vectorVolumeIntegral, mu0 )
import LPFPCore.Current
    ( Current, CurrentDistribution(..)
    , wireToroid, crossedLineIntegral, circularCurrentLoop )

bFieldFromLineCurrent
    :: Current      -- current (in Amps)
    -> Curve
    -> VectorField  -- magnetic field (in Tesla)
bFieldFromLineCurrent i c r
    = let coeff = -mu0 * i / (4 * pi)  -- SI units
          integrand r' = d ^/ magnitude d ** 3
              where d = displacement r' r
      in coeff *^ crossedLineIntegral (curveSample 1000) integrand c

bField :: CurrentDistribution -> VectorField
bField (LineCurrent    i  c) = bFieldFromLineCurrent    i  c
bField (SurfaceCurrent kC s) = bFieldFromSurfaceCurrent kC s
bField (VolumeCurrent  j  v) = bFieldFromVolumeCurrent  j  v
bField (MultipleCurrents cds) = addVectorFields $ map bField cds

circleB :: VectorField  -- magnetic field
circleB = bField $ circularCurrentLoop 0.25 10

bFieldIdealDipole :: Vec          -- magnetic dipole moment
                  -> VectorField  -- magnetic field
bFieldIdealDipole m r
    = let coeff = mu0 / (4 * pi)    -- SI units
          rMag = magnitude (rVF r)
          rUnit = rVF r ^/ rMag
      in coeff *^ (1 / rMag**3) *^ (3 *^ (m <.> rUnit) *^ rUnit ^-^ m)

bFieldWireToroid :: VectorField
bFieldWireToroid = bField (wireToroid 0.3 1 50 10)

bFieldFromSurfaceCurrent
    :: VectorField  -- surface current density
    -> Surface      -- surface across which current flows
    -> VectorField  -- magnetic field (in T)
bFieldFromSurfaceCurrent kCurrent s r
    = let coeff = mu0 / (4 * pi)  -- SI units
          integrand r' = (kCurrent r' >< d) ^/ magnitude d ** 3
              where d = displacement r' r
      in coeff *^ vectorSurfaceIntegral (surfaceSample 200) integrand s

bFieldFromVolumeCurrent
    :: VectorField  -- volume current density
    -> Volume       -- volume throughout which current flows
    -> VectorField  -- magnetic field (in T)
bFieldFromVolumeCurrent j vol r
    = let coeff = mu0 / (4 * pi)  -- SI units
          integrand r' = (j r' >< d) ^/ magnitude d ** 3
              where d = displacement r' r
      in coeff *^ vectorVolumeIntegral (volumeSample 50) integrand vol

magneticFluxFromField :: VectorField -> Surface -> R
magneticFluxFromField = undefined

magneticFluxFromCurrent :: CurrentDistribution -> Surface -> R
magneticFluxFromCurrent = undefined

visLoop :: IO ()
visLoop = undefined
