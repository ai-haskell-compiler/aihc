{-|
Module      : Linear.Geo.Geodetic
Copyright   : Travis Whitaker 2023
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable (Windows, POSIX)

Geodetic coordinates. The ellipsoid is not indexed explicitly, but conversion functions
for WGS84 are provided.

-}

{-# LANGUAGE BangPatterns
           , DeriveAnyClass
           , DeriveDataTypeable
           , DeriveFunctor
           , DeriveGeneric
           , DerivingStrategies
           #-}

module Linear.Geo.Geodetic (
    Geo(..)
  , normalizeGeo
  , fromLatLonAlt
  , toLatLonAlt
  , simpleEllipsoid
  , earthEllipsoid
  , ecefToGeoFerrariEllipsoid
  , ecefToGeoFerrariEarth
  , geoToECEF
  , ecefToGeo
  ) where

import Control.Applicative

import Control.DeepSeq (NFData)

import Control.Monad.Fix
import Control.Monad.Zip

import Data.Data (Data)

import GHC.Generics (Generic)

import qualified Linear.V3 as L

import Linear.Geo.ECEF
import Linear.Geo.PlaneAngle

-- | A point in some geodetic coordinate system, where 'geoLat' is the angle
--   between the normal at the specified point on the ellipsoid and the
--   equatorial plane (north positive, south negative), 'geoLon' is the angle
--   formed by the intersection of the parallel and the prime meridian and the
--   specified point on the parallel, and 'geoAlt' is the magnitude of the
--   position vector minus the magnitude of the unique vector colinear and
--   coordinal with the position vector impingent on the ellipsoid's surface
--   (i.e. height above ellipsoid). Angles are in radians.
data Geo a = Geo {
    geoLat :: !(Radians a)
  , geoLon :: !(Radians a)
  , geoAlt :: !a
  } deriving stock ( Eq
                   , Ord
                   , Show
                   , Generic
                   , Data
                   , Bounded
                   , Functor
                   )
    deriving anyclass (NFData)

instance Applicative Geo where
    pure x = Geo (pure x) (pure x) x
    (Geo pf lf hf) <*> (Geo p l h) = Geo (pf <*> p) (lf <*> l) (hf h)

instance Monad Geo where
    return = pure
    (Geo (Radians p) (Radians l) h) >>= f =
        let Geo p' _ _ = f p
            Geo _ l' _ = f l
            Geo _ _ h' = f h
        in Geo p' l' h'

instance MonadZip Geo where
    mzipWith = liftA2

instance MonadFix Geo where
    mfix f = Geo (let Geo (Radians p) _ _ = f p in Radians p)
                 (let Geo _ (Radians l) _ = f l in Radians l)
                 (let Geo _ _           h = f h in h)

instance Foldable Geo where
    foldMap f (Geo p l h) = foldMap f p <> foldMap f l <> f h

instance Traversable Geo where
    traverse f (Geo p l h) = Geo <$> traverse f p <*> traverse f l <*> f h

-- | Normalize the two angle components of a `Geo`.
normalizeGeo :: (Floating a, Real a) => Geo a -> Geo a
normalizeGeo (Geo p l a) = Geo (normalizeAngle p) (normalizeAngle l) a

-- | Convert a pair of angles and a height above the ellipsoid into a 'Geo'.
fromLatLonAlt :: (PlaneAngle lat, PlaneAngle lon, Floating a, Real a)
              => lat a -- ^ Latitude
              -> lon a -- ^ Longitude
              -> a     -- ^ Altitude
              -> Geo a
fromLatLonAlt lat lon alt = Geo (toRadians lat) (toRadians lon) alt

-- | Unpack a 'Geo' into latitude, longitude, and height above the ellipsoid.
toLatLonAlt :: (PlaneAngle lat, PlaneAngle lon, Floating a, Real a)
            => Geo a
            -> (lat a, lon a, a)
toLatLonAlt (Geo p l a) = (fromRadians p, fromRadians l, a)

-- | Convert from geodetic coordinates to ECEF by assuming the earth is an
--   ellipsoid.
simpleEllipsoid :: Floating a
                => a -- ^ Semi-major axis.
                -> a -- ^ Semi-minor axis.
                -> Geo a
                -> ECEF a
simpleEllipsoid a b =
    let -- coefficient for adjusted prime vertical radius
        dpvr  = (b ^ 2) / (a ^ 2)
        -- square of first eccentricity
        esqr  = 1 - dpvr
        -- prime vertical radius as function of latitude
        pvr p = a / (sqrt (1 - (esqr * ((sin p) ^ 2))))
        proj (Geo (Radians p) (Radians l) h) =
            let n  = pvr p
                nh = n + h
                nd = (dpvr * n) + h
            in ECEF (L.V3 (nh * cos p * cos l)
                          (nh * cos p * sin l)
                          (nd * sin p)
                    )
    in proj

-- | Standard WGS84 ellipsoid.
earthEllipsoid :: RealFloat a
               => Geo a
               -> ECEF a
earthEllipsoid = simpleEllipsoid 6378137 6356752.314245

-- | Conversion from ECEF to geodetic coordinates via a numerically stable
--   formulation of Ferrari's closed-form solution to the quartic polynomial.
--   See https://ieeexplore.ieee.org/document/303772/
ecefToGeoFerrariEllipsoid :: RealFloat a
                          => a -- ^ Semi-major axis.
                          -> a -- ^ Semi-minor axis.
                          -> ECEF a
                          -> Geo a
ecefToGeoFerrariEllipsoid a b (ECEF (L.V3 x y z)) =
    let r     = sqrt ((x ^ 2) + (y ^ 2))
        dpvr  = (b ^ 2) / (a ^ 2)
        esqr  = 1 - dpvr
        e'sqr = ((a ^ 2) - (b ^ 2)) / (b ^ 2)
        eesqr = (a ^ 2) - (b ^ 2)
        ff    = 54 * (b ^ 2) * (z ^ 2)
        gg    = (r ^ 2) + ((1 - esqr) * (z ^ 2)) - (esqr * eesqr)
        cc    = ((esqr ^ 2) * ff * (r ^ 2)) / (gg ^ 3)
        ss    = (1 + cc + sqrt ((cc ^ 2) + (2 * cc))) ** (1 / 3)
        pp    = ff / (3 * (((ss + (1 / ss) + 1)) ^ 2) * (gg ^ 2))
        qq    = sqrt (1 + (2 * (esqr ^ 2) * pp))
        r0    = ((-(pp * esqr * r)) / (1 + qq))
              + (sqrt ( ((1 / 2) * (a ^ 2) * (1 + (1 / qq)))
                      - ((pp * (1 - esqr) * (z ^ 2)) / (qq * (1 + qq)))
                      - ((1 / 2) * pp * (r ^ 2))
                      )
                )
        uu    = sqrt (((r - (esqr * r0)) ^ 2) + (z ^ 2))
        vv    = sqrt (((r - esqr * r0) ^ 2) + ((1 - esqr) * (z ^ 2)))
        zz0   = ((b ^ 2) * z) / (a * vv)
        h     = uu * (1 - ((b ^ 2) / (a * vv)))
        p     = atan ((z + (e'sqr * zz0)) / r)
        l     = atan2 y x
    in Geo (Radians p) (Radians l) h

-- | Standard WGS84 ellipsoid.
ecefToGeoFerrariEarth :: RealFloat a => ECEF a -> Geo a
ecefToGeoFerrariEarth = ecefToGeoFerrariEllipsoid 6378137 6356752.314245

-- | Synonym for 'earthEllipsoid'.
geoToECEF :: RealFloat a => Geo a -> ECEF a
geoToECEF = earthEllipsoid

-- | Synonym for 'ecefToGeoFerrariEarth'.
ecefToGeo ::  RealFloat a => ECEF a -> Geo a
ecefToGeo = ecefToGeoFerrariEarth
