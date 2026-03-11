{-|
Module      : Linear.Geo.ENU
Copyright   : Travis Whitaker 2023
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable (Windows, POSIX)

East-North-Up coordinates.

-}

{-# LANGUAGE DataKinds
           , DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           , DerivingStrategies
           , MagicHash
           , ScopedTypeVariables
           , TypeFamilies
           #-}

module Linear.Geo.ENU (
    ENU(..)
  , alignOrigin
  , liftAO2
  , liftAO2V
  , rotNormToECEF
  , rotNormToECEFFromENU
  , enuToECEF
  , rotECEFToNorm
  , rotECEFToNormFromENU
  , ecefToENU
  , disp
  , diff
  , lerp
  , dot
  , quadrance
  , norm
  , distance
  , normalize
  , project
  ) where

import Control.DeepSeq (NFData)

import Data.Data (Data)

import GHC.Generics (Generic)

import GHC.Exts

import qualified Linear.Affine  as L
import qualified Linear.Epsilon as L
import qualified Linear.Matrix  as L
import qualified Linear.Metric  as L
import qualified Linear.V2      as L
import qualified Linear.V3      as L
import qualified Linear.Vector  as L

import Linear.Geo.ECEF
import Linear.Geo.Geodetic
import Linear.Geo.PlaneAngle

-- | R3 vector with the origin located at some arbitrary 'ECEF' position vector,
--   first basis pointing east at the origin, second basis vector pointing north
--   at the origin, and third basis vector normal to the plane tangent to the
--   ellipsoid at the origin.
--
--   Each value records both the ENU vector and the ENU origin. Most functions
--   of multiple ENU values will require the points to occupy coordinal frames.
--   Binary operations on ENU values should preserve the coordinate frame of the
--   /left/ value.
--
--   The 'Eq' and 'Ord' instances for this type implement structural equality,
--   i.e. ENU points with different 'enuOrigin' values will never be equal.
--   Floating point errors limit the usefulness of
--   exact-equality-as-coincidence.
--
--   Operations on ENU points use the uncorrected WGS84 geoid model.
data ENU a = ENU {
    enuOrigin :: ECEF a
  , enuPoint  :: L.V3 a
  } deriving stock ( Eq
                   , Ord
                   , Show
                   , Generic
                   , Data
                   , Bounded
                   )
    deriving anyclass (NFData)

instance L.R1 ENU where
    _x f (ENU o (L.V3 x y z)) = (\x' -> ENU o (L.V3 x' y z)) <$> f x

instance L.R2 ENU where
    _y  f (ENU o (L.V3 x y z)) = (\y' -> ENU o (L.V3 x y' z)) <$> f y
    _xy f (ENU o (L.V3 x y z)) = (\(L.V2 x' y') -> ENU o (L.V3 x' y' z))
                             <$> f (L.V2 x y)

instance L.R3 ENU where
    _z   f (ENU o (L.V3 x y z)) = (\z' -> ENU o (L.V3 x y z')) <$> f z
    _xyz f (ENU o v)            = ENU o <$> f v

-- | Align the second argument with the coordinate system of the first.
alignOrigin :: RealFloat a => ENU a -> ENU a -> ENU a
alignOrigin (ENU xo _) y@(ENU yo _)
    | isTrue# (reallyUnsafePtrEquality# xo yo) = y
    | xo == yo  = y
    | otherwise = ecefToENU xo (enuToECEF y)

-- | Lift a function on vectors to a function on origin-aligned ENU points.
liftAO2 :: RealFloat a => (L.V3 a -> L.V3 a -> b) -> ENU a -> ENU a -> b
liftAO2 f x@(ENU _ xp) y = let (ENU _ y'p) = alignOrigin x y
                           in f xp y'p

-- | Lift a binary operation on vectors to a binary operation on origin-aligned
--   ENU points.
liftAO2V :: RealFloat a
         => (L.V3 a -> L.V3 a -> L.V3 a)
         -> ENU a
         -> ENU a
         -> ENU a
liftAO2V f x@(ENU xo xp) y = let (ENU _ y'p) = alignOrigin x y
                             in ENU xo (f xp y'p)

-- | Rotation matrix that rotates the ENU coordinate frame at the provided
--   latitude and longitude to the ECEF coordinate frame.
rotNormToECEF :: Floating a
              => Radians a -- ^ lat
              -> Radians a -- ^ lon
              -> L.M33 a
rotNormToECEF (Radians po) (Radians lo) =
    L.V3 (L.V3 (-(sin lo)) ((-(cos lo)) * (sin po))  ((cos lo) * (cos po)))
         (L.V3 (cos lo)    ((- (sin lo)) * (sin po)) ((sin lo) * (cos po)))
         (L.V3 0           (cos po)                  (sin po)             )

-- | Do 'rotNormToECEF', but get the lat and lon from some 'ENU's origin.
rotNormToECEFFromENU :: RealFloat a => ENU a -> L.M33 a
rotNormToECEFFromENU (ENU o _) =
    let (Geo po lo _) = ecefToGeo o
    in rotNormToECEF po lo

-- | Convert an 'ENU' to an 'ECEF' by adding the rotated position vector to the
--   origin.
enuToECEF :: RealFloat a => ENU a -> ECEF a
enuToECEF enu@(ENU o x) =
    let rot = rotNormToECEFFromENU enu
    in o L..+^ (rot L.!* x)

-- | Rotation matrix that rotates the ECEF coordinate frame to the ENU
--   coordinate frame at the provided latitude and longitude.
rotECEFToNorm :: Floating a
              => Radians a -- ^ lat
              -> Radians a -- ^ lon
              -> L.M33 a
rotECEFToNorm (Radians po) (Radians lo) =
    L.V3 (L.V3 (-(sin lo))              (cos lo)                 0       )
         (L.V3 ((-(cos lo)) * (sin po)) ((-(sin lo)) * (sin po)) (cos po))
         (L.V3 ((cos lo) * (cos po))    ((sin lo) * (cos po))    (sin po))

-- | Do 'rotECEFToNorm', but get the lat and lon from some 'ENU's origin.
rotECEFToNormFromENU :: RealFloat a => ENU a -> L.M33 a
rotECEFToNormFromENU (ENU o _) =
    let (Geo po lo _) = ecefToGeo o
    in rotECEFToNorm po lo

-- | Pack an 'ECEF' origin and point into an 'ENU'. 
ecefToENU :: RealFloat a
          => ECEF a -- ^ Origin
          -> ECEF a -- ^ Point
          -> ENU a
ecefToENU o@(ECEF vo) (ECEF vp) =
    let (Geo po lo _) = ecefToGeo o
        rot = rotECEFToNorm po lo
        x = rot L.!* (vp - vo)
    in ENU o x

-- | Affine addition. Apply a displacement vector.
disp :: Num a => ENU a -> L.V3 a -> ENU a
disp (ENU o p) v = (ENU o (p + v))

-- | Affine subtraction. Get the vector from the first to the second ENU point.
diff :: RealFloat a => ENU a -> ENU a -> L.V3 a
diff x y = enuPoint $ liftAO2V (L..-.) x y

-- | Linearly interpolate between two points.
lerp :: RealFloat a => a -> ENU a -> ENU a -> ENU a
lerp f = liftAO2V (L.lerp f)

-- | Lifted dot.
dot :: RealFloat a => ENU a -> ENU a -> a
dot = liftAO2 L.dot

-- | Lifted quadrance.
quadrance :: Num a => ENU a -> a
quadrance = L.quadrance . enuPoint

-- | Lifted norm.
norm :: Floating a => ENU a -> a
norm = L.norm . enuPoint

-- | Lifted distance.
distance :: RealFloat a => ENU a -> ENU a -> a
distance = liftAO2 L.distance

-- | Lifted normalize.
normalize :: (Floating a, L.Epsilon a) => ENU a -> ENU a
normalize (ENU xo xp) = ENU xo (L.normalize xp)

-- | Lifted project.
project :: RealFloat a => ENU a -> ENU a -> ENU a
project = liftAO2V L.project
