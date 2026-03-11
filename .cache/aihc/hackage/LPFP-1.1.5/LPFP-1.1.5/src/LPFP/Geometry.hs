{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.Geometry
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 23 of the book Learn Physics with Functional Programming
-}

module LPFP.Geometry where

import LPFP.SimpleVec ( R, Vec, (*^) )
import LPFP.CoordinateSystems ( Position, cylindrical, spherical, cart, cyl, sph
                         , shiftPosition, displacement )

data Curve = Curve { curveFunc          :: R -> Position
                   , startingCurveParam :: R  -- t_a
                   , endingCurveParam   :: R  -- t_b
                   }

circle2 :: Curve
circle2 = Curve (\t -> cart (2 * cos t) (2 * sin t) 0) 0 (2*pi)

circle2' :: Curve
circle2' = Curve (\phi -> cyl 2 phi 0) 0 (2*pi)

unitCircle :: Curve
unitCircle = Curve (\t -> cyl 1 t 0) 0 (2 * pi)

straightLine :: Position  -- starting position
             -> Position  -- ending position
             -> Curve     -- straight-line curve
straightLine r1 r2 = let d = displacement r1 r2
                         f t = shiftPosition (t *^ d) r1
                     in Curve f 0 1

data Surface = Surface { surfaceFunc :: (R,R) -> Position
                       , lowerLimit  :: R       -- s_l
                       , upperLimit  :: R       -- s_u
                       , lowerCurve  :: R -> R  -- t_l(s)
                       , upperCurve  :: R -> R  -- t_u(s)
                       }

unitSphere :: Surface
unitSphere = Surface (\(th,phi) -> cart (sin th * cos phi)
                                        (sin th * sin phi)
                                        (cos th))
                     0 pi (const 0) (const $ 2*pi)

unitSphere' :: Surface
unitSphere' = Surface (\(th,phi) -> sph 1 th phi)
                      0 pi (const 0) (const $ 2*pi)

parabolaSurface :: Surface
parabolaSurface = Surface (\(x,y) -> cart x y 0)
                          (-2) 2 (\x -> x*x) (const 4)

shiftSurface :: Vec -> Surface -> Surface
shiftSurface d (Surface g sl su tl tu)
    = Surface (shiftPosition d . g) sl su tl tu

centeredSphere :: R -> Surface
centeredSphere r = Surface (\(th,phi) -> sph r th phi)
                           0 pi (const 0) (const $ 2*pi)

sphere :: R -> Position -> Surface
sphere radius center
    = shiftSurface (displacement (cart 0 0 0) center)
      (centeredSphere radius)

northernHemisphere :: Surface
northernHemisphere = Surface (\(th,phi) -> sph 1 th phi)
                             0 (pi/2) (const 0) (const $ 2*pi)

disk :: R -> Surface
disk radius = Surface (\(s,phi) -> cyl s phi 0)
                      0 radius (const 0) (const (2*pi))

unitCone :: R -> Surface
unitCone theta = Surface (\(r,phi) -> sph r theta phi)
                         0 1 (const 0) (const (2*pi))

data Volume = Volume { volumeFunc :: (R,R,R) -> Position
                     , loLimit    :: R            -- s_l
                     , upLimit    :: R            -- s_u
                     , loCurve    :: R -> R       -- t_l(s)
                     , upCurve    :: R -> R       -- t_u(s)
                     , loSurf     :: R -> R -> R  -- u_l(s,t)
                     , upSurf     :: R -> R -> R  -- u_u(s,t)
                     }

unitBall :: Volume
unitBall = Volume spherical 0 1 (const 0) (const pi)
                  (\_ _ -> 0) (\_ _ -> 2*pi)

centeredCylinder :: R       -- radius
                 -> R       -- height
                 -> Volume  -- cylinder
centeredCylinder radius height
  = Volume cylindrical 0 radius (const 0) (const (2*pi))
           (\_ _ -> 0) (\_ _ -> height)

circle :: Position  -- center position
       -> R         -- radius
       -> Curve
circle r radius = undefined r radius

square :: Curve
square = Curve squareFunc 0 4

squareFunc :: R -> Position
squareFunc t
    |           t < 1  = cart undefined    (-1)   0
    | 1 <= t && t < 2  = cart     1     undefined 0
    | 2 <= t && t < 3  = cart undefined      1    0
    | otherwise        = cart   (-1)    undefined 0

northernHalfBall :: Volume
northernHalfBall = undefined

centeredBall :: R -> Volume
centeredBall = undefined

shiftVolume :: Vec -> Volume -> Volume
shiftVolume = undefined

quarterDiskBoundary :: R -> Curve
quarterDiskBoundary = undefined

quarterCylinder :: R -> R -> Volume
quarterCylinder = undefined
