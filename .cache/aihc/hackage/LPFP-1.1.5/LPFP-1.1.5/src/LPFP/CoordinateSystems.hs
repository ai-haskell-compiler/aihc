{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.CoordinateSystems
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 22 of the book Learn Physics with Functional Programming
-}

module LPFP.CoordinateSystems where

import LPFP.SimpleVec
    ( R, Vec, (^/), vec, xComp, yComp, zComp, iHat, jHat, kHat
    , magnitude, sumV, zeroV )
import LPFP.Mechanics3D ( orient, v3FromVec )
import LPFP.MOExamples ( Table(..), Justification(..) )
import qualified Vis as V
import Linear ( V3(..) )
import Diagrams.Prelude
    ( Diagram, V2(..), PolyType(..), PolyOrientation(..), PolygonOpts(..)
    , (#), (@@), dims, p2, r2, arrowAt, position, fc, black, white
    , blend, none, lw, rotate, deg, rad, scale, polygon, sinA )
import Diagrams.Backend.Cairo ( B, renderCairo )

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

sf3D :: [Position]   -- positions to use
     -> ScalarField  -- to display
     -> IO ()
sf3D ps sf
    = V.display whiteBackground $ orient $
      V.VisObjects [V.Text3d (show (round $ sf p :: Int))
                    (v3FromPos p) V.Fixed9By15 V.black
                        | p <- ps]

v3FromPos :: Position -> V3 R
v3FromPos p = V3 x y z
    where
      (x,y,z) = cartesianCoordinates p

whiteBackground :: V.Options
whiteBackground = V.defaultOpts {V.optBackgroundColor = Just V.white}

whiteBackground' :: V.Options
whiteBackground'
    = V.defaultOpts {V.optBackgroundColor = Just V.white,
                     V.optInitialCamera   = Just V.Camera0 {V.rho0   = 40.0,
                                                            V.theta0 = 45.0,
                                                            V.phi0   = 20.0}}

ySF3D :: IO ()
ySF3D = sf3D [cart x y z | x <- [-6,-2..6]
                         , y <- [-6,-2..6]
                         , z <- [-6,-2..6]] ySF

sfTable :: ((R,R) -> Position)
        -> [R]  -- horizontal
        -> [R]  -- vertical
        -> ScalarField
        -> Table Int
sfTable toPos ss ts sf
    = Table RJ [[round $ sf $ toPos (s,t) | s <- ss] | t <- reverse ts]

vf3D :: R            -- scale factor, vector field units per meter
     -> [Position]   -- positions to show the field
     -> VectorField  -- vector field to display
     -> IO ()
vf3D unitsPerMeter ps vf
    = V.display whiteBackground $ orient $
      V.VisObjects [V.Trans (v3FromPos p) $
                    visVec V.black (vf p ^/ unitsPerMeter)
                        | p <- ps]

visVec :: V.Color -> Vec -> V.VisObject R
visVec color v = let vmag = magnitude v
                 in V.Arrow (vmag,20*vmag) (v3FromVec v) color

phiHat3D :: IO ()
phiHat3D = vf3D 1 [cyl r ph z | r  <- [1,2,3]
                              , ph <- [0,pi/4..2*pi]
                              , z  <- [-2..2]] phiHat

vfPNG :: ((R,R) -> Position)
      -> (Vec -> (R,R))
      -> FilePath     -- file name
      -> R            -- scale factor in units per meter
      -> [(R,R)]      -- positions to use
      -> VectorField
      -> IO ()
vfPNG toPos fromVec fileName unitsPerMeter pts vf
    = let vf2d = r2 . fromVec . (^/ unitsPerMeter) . vf . toPos
          pic  = mconcat [arrowAt (p2 pt) (vf2d pt) | pt <- pts]
      in renderCairo fileName (dims (V2 1024 1024)) pic

vfPNGxy :: FilePath     -- file name
        -> R            -- scale factor
        -> [(R,R)]      -- positions to use
        -> VectorField
        -> IO ()
vfPNGxy = vfPNG (\(x,y) -> cart x y 0) (\v -> (xComp v, yComp v))

phiHatPNG :: IO ()
phiHatPNG
    = vfPNGxy "phiHatPNG.png" 1
      [(r * cos ph, r * sin ph) | r  <- [1,2]
                                , ph <- [0,pi/4..2*pi]] phiHat

rVFpng :: IO ()
rVFpng
    = vfPNGxy "rVFpng.png" 2
      [(r * cos ph, r * sin ph) | r  <- [1,2]
                                , ph <- [0,pi/4..2*pi]] rVF

vfGrad :: (R -> R)
       -> ((R,R) -> Position)
       -> (Vec -> (R,R))
       -> FilePath
       -> Int    -- n for n x n
       -> VectorField
       -> IO ()
vfGrad curve toPos fromVec fileName n vf 
  = let step = 2 / fromIntegral n            
        xs = [-1+step/2, -1+3*step/2 .. 1-step/2]
        pts = [(x, y) | x <- xs, y <- xs]
        array = [(pt,magRad $ fromVec $ vf $ toPos pt) | pt <- pts] 
        maxMag = maximum (map (fst . snd) array) 
        scaledArrow m th = scale step $ arrowMagRad (curve (m/maxMag)) th
        pic = position [(p2 pt, scaledArrow m th) | (pt,(m,th)) <- array] 
     in renderCairo fileName (dims (V2 1024 1024)) pic 

magRad :: (R,R) -> (R,R)
magRad (x,y) = (sqrt (x*x + y*y), atan2 y x)

-- magnitude from 0 to 1
arrowMagRad :: R  -- magnitude
            -> R  -- angle in radians, counterclockwise from x axis
            -> Diagram B
arrowMagRad mag th
    = let r      = sinA (15 @@ deg) / sinA (60 @@ deg)
          myType = PolyPolar [120 @@ deg, 0 @@ deg, 45 @@ deg, 30 @@ deg,
                              45 @@ deg, 0 @@ deg, 120 @@ deg]
                   [1,1,r,1,1,r,1,1]
          myOpts = PolygonOpts myType NoOrient (p2 (0,0))
      in scale 0.5 $ polygon myOpts # lw none # fc (blend mag black white) #
         rotate (th @@ rad)

rVFGrad :: IO ()
rVFGrad = vfGrad id
          (\(x,y) -> cart x y 0)
          (\v -> (xComp v,yComp v))
          "rVFGrad.png" 20
          rVF

thetaSF :: ScalarField
thetaSF = undefined

thetaHat3D :: IO ()
thetaHat3D = undefined

thetaHatGrad :: IO ()
thetaHatGrad = vfGrad id undefined undefined "thetaHatGrad.png" 20 thetaHat

phiHatGrad :: IO ()
phiHatGrad = undefined
