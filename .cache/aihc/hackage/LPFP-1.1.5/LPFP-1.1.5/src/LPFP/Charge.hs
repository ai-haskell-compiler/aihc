{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.Charge
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 24 of the book Learn Physics with Functional Programming
-}

module LPFP.Charge where

import LPFP.SimpleVec ( R, Vec, vec, sumV, (*^), (^/), (<.>), magnitude, negateV )
import LPFP.Electricity ( elementaryCharge )
import LPFP.CoordinateSystems ( Position, ScalarField, origin, cart, sph
                         , rVF, displacement, shiftPosition )
import LPFP.Geometry ( Curve(..), Surface(..), Volume(..)
                , straightLine, shiftSurface, disk )
import LPFP.Integrals
    ( scalarLineIntegral, scalarSurfaceIntegral, scalarVolumeIntegral
    , vectorLineIntegral, vectorSurfaceIntegral, vectorVolumeIntegral
    , curveSample, surfaceSample, volumeSample )

type Charge = R

data ChargeDistribution
    = PointCharge   Charge      Position
    | LineCharge    ScalarField Curve
    | SurfaceCharge ScalarField Surface
    | VolumeCharge  ScalarField Volume
    | MultipleCharges [ChargeDistribution]

protonOrigin :: ChargeDistribution
protonOrigin = PointCharge elementaryCharge origin

chargedLine :: Charge -> R -> ChargeDistribution
chargedLine q len
    = LineCharge (const $ q / len) $
      Curve (\z -> cart 0 0 z) (-len/2) (len/2)

chargedBall :: Charge -> R -> ChargeDistribution
chargedBall q radius
    = VolumeCharge (const $ q / (4/3*pi*radius**3)) $
      Volume (\(r,theta,phi) -> sph r theta phi)
                 0 radius (const 0) (const pi) (\_ _ -> 0) (\_ _ -> 2*pi)

diskCap :: R -> R -> R -> ChargeDistribution
diskCap radius plateSep sigma
    = MultipleCharges
      [SurfaceCharge (const sigma) $
       shiftSurface (vec 0 0 (plateSep/2)) (disk radius)
      ,SurfaceCharge (const $ -sigma) $
       shiftSurface (vec 0 0 (-plateSep/2)) (disk radius)
      ]

totalCharge :: ChargeDistribution -> Charge
totalCharge (PointCharge   q      _)
    = q
totalCharge (LineCharge    lambda c)
    = scalarLineIntegral    (curveSample  1000) lambda c
totalCharge (SurfaceCharge sigma  s)
    = scalarSurfaceIntegral (surfaceSample 200) sigma s
totalCharge (VolumeCharge  rho    v)
    = scalarVolumeIntegral  (volumeSample   50) rho v
totalCharge (MultipleCharges ds    )
    = sum [totalCharge d | d <- ds]

simpleDipole :: Vec  -- electric dipole moment
             -> R    -- charge separation
             -> ChargeDistribution
simpleDipole p sep
    = let q    = magnitude p / sep
          disp = (sep/2) *^ (p ^/ magnitude p)
      in MultipleCharges
             [PointCharge   q  (shiftPosition          disp  origin)
             ,PointCharge (-q) (shiftPosition (negateV disp) origin)
             ]

electricDipoleMoment :: ChargeDistribution -> Vec
electricDipoleMoment (PointCharge   q      r)
    = q *^ displacement origin r
electricDipoleMoment (LineCharge    lambda c)
    = vectorLineIntegral    (curveSample  1000) (\r -> lambda r *^ rVF r) c
electricDipoleMoment (SurfaceCharge sigma  s)
    = vectorSurfaceIntegral (surfaceSample 200) (\r -> sigma  r *^ rVF r) s
electricDipoleMoment (VolumeCharge  rho    v)
    = vectorVolumeIntegral  (volumeSample   50) (\r -> rho    r *^ rVF r) v
electricDipoleMoment (MultipleCharges ds    )
    = sumV [electricDipoleMoment d | d <- ds]

lineDipole :: Vec  -- dipole moment
           -> R    -- charge separation
           -> ChargeDistribution
lineDipole p sep
    = let disp = (sep/2) *^ (p ^/ magnitude p)
          curve = straightLine (shiftPosition (negateV disp) origin)
                               (shiftPosition          disp  origin)
          coeff = 12 / sep**3
          lambda r = coeff * (displacement origin r <.> p)
      in LineCharge lambda curve

chargedDisk :: Charge -> R -> ChargeDistribution
chargedDisk q radius = undefined q radius

circularLineCharge :: Charge -> R -> ChargeDistribution
circularLineCharge q radius = undefined q radius

chargedSquarePlate :: Charge -> R -> ChargeDistribution
chargedSquarePlate q side = undefined q side

chargedSphericalShell :: Charge -> R -> ChargeDistribution
chargedSphericalShell q radius = undefined q radius

chargedCube :: Charge -> R -> ChargeDistribution
chargedCube q side = undefined q side

squareCap :: R -> R -> R -> ChargeDistribution
squareCap side plateSep sigma = undefined side plateSep sigma

hydrogen :: ChargeDistribution
hydrogen = undefined
