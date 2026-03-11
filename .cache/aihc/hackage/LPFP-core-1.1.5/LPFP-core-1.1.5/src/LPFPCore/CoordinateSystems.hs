{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFPCore.CoordinateSystems
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 22 of the book Learn Physics with Functional Programming
-}

module LPFPCore.CoordinateSystems where

import LPFPCore.SimpleVec
    ( R, Vec, (^/), vec, xComp, yComp, zComp, iHat, jHat, kHat
    , magnitude, sumV, zeroV )
import LPFPCore.MOExamples ( Table(..), Justification(..) )

data Position = Cart R R R
                deriving (Show)

type CoordinateSystem = (R,R,R) -> Position

cartesian   :: CoordinateSystem
cartesian (x,y,z)
    = Cart x y z

cylindrical :: CoordinateSystem
cylindrical (s,phi,z)
    = Cart (s * cos phi) (s * sin phi) z

spherical   :: CoordinateSystem
spherical (r,theta,phi)
    = Cart (r * sin theta * cos phi)
           (r * sin theta * sin phi)
           (r * cos theta)

cart :: R  -- x coordinate
     -> R  -- y coordinate
     -> R  -- z coordinate
     -> Position
cart = Cart

cyl  :: R  -- s   coordinate
     -> R  -- phi coordinate
     -> R  -- z   coordinate
     -> Position
cyl s phi z = cylindrical (s,phi,z)

sph  :: R  -- r     coordinate
     -> R  -- theta coordinate
     -> R  -- phi   coordinate
     -> Position
sph r theta phi = spherical (r,theta,phi)

origin :: Position
origin = cart 0 0 0

cartesianCoordinates   :: Position -> (R,R,R)
cartesianCoordinates   (Cart x y z) = (x,y,z)

cylindricalCoordinates :: Position -> (R,R,R)
cylindricalCoordinates (Cart x y z) = (s,phi,z)
    where
      s = sqrt(x**2 + y**2)
      phi = atan2 y x

sphericalCoordinates   :: Position -> (R,R,R)
sphericalCoordinates   (Cart x y z) = (r,theta,phi)
    where
      r = sqrt(x**2 + y**2 + z**2)
      theta = atan2 s z
      s = sqrt(x**2 + y**2)
      phi = atan2 y x

type Displacement = Vec

displacement :: Position  -- source position
             -> Position  -- target position
             -> Displacement
displacement (Cart x' y' z') (Cart x y z)
    = vec (x-x') (y-y') (z-z')

shiftPosition :: Displacement -> Position -> Position
shiftPosition v (Cart x y z)
  = Cart (x + xComp v) (y + yComp v) (z + zComp v)

type ScalarField = Position -> R

xSF :: ScalarField
xSF p = x
    where
      (x,_,_) = cartesianCoordinates p

rSF :: ScalarField
rSF p = r
    where
      (r,_,_) = sphericalCoordinates p

fst3 :: (a,b,c) -> a
fst3 (u,_,_) = u

snd3 :: (a,b,c) -> b
snd3 (_,u,_) = u

thd3 :: (a,b,c) -> c
thd3 (_,_,u) = u

ySF :: ScalarField
ySF = snd3 . cartesianCoordinates

type VectorField = Position -> Vec

sHat   :: VectorField
sHat   r = vec ( cos phi) (sin phi) 0
    where
      (_,phi,_) = cylindricalCoordinates r

phiHat :: VectorField
phiHat r = vec (-sin phi) (cos phi) 0
    where
      (_,phi,_) = cylindricalCoordinates r

rHat :: VectorField
rHat rv = let d = displacement origin rv
          in if d == zeroV
             then zeroV
             else d ^/ magnitude d

thetaHat :: VectorField
thetaHat r = vec ( cos theta * cos phi)
                 ( cos theta * sin phi)
                 (-sin theta          )
    where
      (_,theta,phi) = sphericalCoordinates r

xHat :: VectorField
xHat = const iHat

yHat :: VectorField
yHat = const jHat

zHat :: VectorField
zHat = const kHat

rVF :: VectorField
rVF = displacement origin

addScalarFields :: [ScalarField] -> ScalarField
addScalarFields flds r = sum  [fld r | fld <- flds]

addVectorFields :: [VectorField] -> VectorField
addVectorFields flds r = sumV [fld r | fld <- flds]

sfTable :: ((R,R) -> Position)
        -> [R]  -- horizontal
        -> [R]  -- vertical
        -> ScalarField
        -> Table Int
sfTable toPos ss ts sf
    = Table RJ [[round $ sf $ toPos (s,t) | s <- ss] | t <- reverse ts]

magRad :: (R,R) -> (R,R)
magRad (x,y) = (sqrt (x*x + y*y), atan2 y x)

thetaSF :: ScalarField
thetaSF = undefined

thetaHat3D :: IO ()
thetaHat3D = undefined

phiHatGrad :: IO ()
phiHatGrad = undefined
