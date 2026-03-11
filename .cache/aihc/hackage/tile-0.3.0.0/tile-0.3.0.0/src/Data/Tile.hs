{-|
Module      : Data.Tile
Description : Tile/lonlat conversion utilities for slippy maps.
Copyright   : (c) Joe Canero, 2017
License     : BSD3
Maintainer  : jmc41493@gmail.com
Stability   : experimental
Portability : POSIX

This module provides types and functions for manipulating tiles,
latitude/longitude pairs, and pixels.

See the associated README.md for basic usage examples.
-}

module Data.Tile
(
  -- * Types
  Z(..)
, X(..)
, Y(..)
, Tile(..)
, Lng(..)
, Lat(..)
, LngLat(..)
, Px(..)
, Py(..)
, Pixel(..)
, BoxOrigin(..)
, TileBounds(..)
-- * Functions
-- ** Tile functionality
, maxTileIndex
, maxTilesInDimension
, maxPixelsInDimension
, flipY
, subTiles
, parentTile
, mkTile
, tilesInBounds
-- ** Conversions
-- *** To Pixel
, tileToPixel
, lngLatToPixel
-- *** To Tile
, pixelToTile
, lngLatToTile
-- *** To LngLat
, pixelToLngLat
, tileToLngLat
-- *** To TileBounds
, tileToBounds
) where

import           Data.Bifunctor
import           Data.Bits

-- | Newtype wrapper around map zoom level.
newtype Z = Z Int
  deriving (Show, Eq, Ord)

-- | Newtype wrapper around a tile's x-coordinate.
newtype X = X Int
  deriving (Show, Eq, Ord)

-- | Newtype wrapper around a tile's y-coordinate.
newtype Y = Y Int
  deriving (Show, Eq, Ord)

-- | Newtype wrapper around a triple of 'Z', 'X', and 'Y' representing
-- a single tile in a map's tile system.
newtype Tile = Tile (Z, X, Y)
  deriving (Show, Eq, Ord)

-- | Newtype wrapper around longitude.
newtype Lng = Lng Double
  deriving (Show, Eq, Ord)

instance Bounded Lng where
  minBound = Lng (-180.0)
  maxBound = Lng 180.0

-- | Newtype wrapper around latitude.
newtype Lat = Lat Double
  deriving (Show, Eq, Ord)

instance Bounded Lat where
  minBound = Lat (-90.0)
  maxBound = Lat 90.0

-- | Newtype wrapper around a tuple of 'Lng' and 'Lat' representing
-- a WGS84 latitude and longitude on the map.
newtype LngLat = LngLat (Lng, Lat)
  deriving (Show, Eq, Ord)

-- | Newtype wrapper around a pixel's x-coordinate
newtype Px = Px Int
  deriving (Show, Eq, Ord)

-- | Newtype wrapper around a pixel's y-coordinate
newtype Py = Py Int
  deriving (Show, Eq, Ord)

-- | Newtype wrapper around a tuple of 'Px' and 'Py' representing
-- a single pixel.
newtype Pixel = Pixel (Px, Py)
  deriving (Show, Eq, Ord)

-- | Datatype representing the origin point of a bounding box.
data BoxOrigin = SW -- ^ Indicates that the origin is in the southwest corner of the bbox, and the
                    --   second point is in the northeast corner of the bbox.
               | NW -- ^ Indicates that the origin is in the northwest corner of the bbox, and the
                    --   second point is in the southeast corner of the bbox.
  deriving (Show, Eq)

-- | Datatype representing the bounds of a tile as a 'BoxOrigin' and two 'LngLat' values.
data TileBounds = TileBounds BoxOrigin LngLat LngLat
  deriving (Show, Eq)

-- | Get the maximum index a tile can have along a given dimension, x or y.
maxTileIndex :: Z -> Int
maxTileIndex z = maxTilesInDimension z - 1

-- | Get the numbers of tiles in a given dimension, x or y, at the specified
-- map zoom level.
maxTilesInDimension :: Z -> Int
maxTilesInDimension (Z z) = 1 `shift` z

-- | Get the number of pixels in a given dimension, x or y, assuming
-- a tile is 256x256px.
maxPixelsInDimension :: Z -> Int
maxPixelsInDimension = (256 *) . maxTilesInDimension

-- | Smart constructor for tiles. Validates that
-- the 'X' and 'Y' values are valid for the value of 'Z'.
mkTile :: Z -> X -> Y -> Maybe Tile
mkTile zv@(Z z) xv@(X x) yv@(Y y)
  | 0 <= x && x <= tileMax &&
    0 <= y && y <= tileMax    = Just $ Tile (zv, xv, yv)
  | otherwise                 = Nothing
  where tileMax = maxTileIndex zv

-- | Convert a 'Tile' to a 'Pixel' assuming
-- a tile size of 256x256px.
tileToPixel :: Tile -> Pixel
tileToPixel (Tile (Z z, X x, Y y)) = Pixel (Px $ 256 * x, Py $ 256 * y)

-- | Convert a 'LngLat' into a 'Pixel'.
lngLatToPixel :: Z -> LngLat -> Pixel
lngLatToPixel z (LngLat (lng, lat)) =
  let (Lng lng') = clipBounded lng
      (Lat lat') = clipBounded lat
      x      = (lng' + 180.0) / 360.0
      sinlat = sin (lat' * pi / 180.0)
      y      = 0.5 - log ((1 + sinlat) / (1 - sinlat)) / (4 * pi)
      px     = floor $ min (maxPixels - 1) $ max 0 (x * maxPixels + 0.5)
      py     = floor $ min (maxPixels - 1) $ max 0 (y * maxPixels + 0.5)
    in Pixel (Px px, Py py)
  where maxPixels = fromIntegral $ maxPixelsInDimension z

-- | Convert a 'Pixel' into a 'Tile' assuming
-- a tile size of 256x256px.
pixelToTile :: Z -> Pixel -> Tile
pixelToTile z (Pixel (Px px, Py py)) = Tile (z, X $ px `div` 256, Y $ py `div` 256)

-- | Convert a 'LngLat' into a 'Tile'.
lngLatToTile :: Z -> LngLat -> Tile
lngLatToTile z = pixelToTile z . lngLatToPixel z

-- | Convert a 'Pixel' into a 'LngLat' assuming
-- map resolution as defined at the equator.
pixelToLngLat :: Z -> Pixel -> LngLat
pixelToLngLat z p =
  let x' = fromIntegral x / fromIntegral maxPx - 0.5
      y' = 0.5 - fromIntegral y / fromIntegral maxPx
      lng = 360 * x'
      lat = 90 - 360 * atan (exp ((-y') * 2 * pi)) / pi
    in LngLat (Lng lng, Lat lat)
  where (Pixel (Px x, Py y)) = clip z p
        maxPx = maxPixelsInDimension z

-- | Convert a 'Tile' into a 'LngLat'.
tileToLngLat :: Tile -> LngLat
tileToLngLat t@(Tile (z, _, _)) = pixelToLngLat z $ tileToPixel t

-- | Convert a 'Tile' into a 'TileBounds' value representing
-- the bounding box of that tile.
tileToBounds :: BoxOrigin -> Tile -> TileBounds
tileToBounds SW t@(Tile (z, X x, Y y)) = TileBounds SW (tileToLngLat (Tile (z, X x, Y $ y + 1)))
                                                       (tileToLngLat (Tile (z, X $ x + 1, Y y)))
tileToBounds NW t@(Tile (z, X x, Y y)) = TileBounds NW (tileToLngLat (Tile (z, X x, Y y)))
                                                       (tileToLngLat (Tile (z, X $ x + 1, Y $ y + 1)))

-- | Given a 'Tile', return a list of its four subtiles.
subTiles :: Tile -> [Tile]
subTiles (Tile (Z z, X x, Y y)) = [Tile (Z $ z + 1, X $ 2 * x + i, Y $ 2 * y + j) | i <- [0..1], j <- [0..1]]

-- | Given a 'Tile', return its parent 'Tile', if it has one.
parentTile :: Tile -> Maybe Tile
parentTile (Tile (Z z, X x, Y y))
  | z == 0    = Nothing
  | otherwise = Just $ Tile (Z $ z - 1, X $ x `div` 2, Y $ y `div` 2)

-- | Given a 'Tile', flip its y-coordinate according to the rules of TMS.
flipY :: Tile -> Tile
flipY (Tile (z, x, Y y)) = Tile (z, x, Y $ maxTileIndex z - y)

-- | Given a 'BoxOrigin', a 'Z', and a pair of 'LngLat's, return a list of all
-- tiles touching or within the bounding box.
tilesInBounds :: BoxOrigin -> Z -> (LngLat, LngLat) -> [Tile]
tilesInBounds bo z ll@(LngLat (lng1, lat1), LngLat (lng2, lat2)) =
  let (ll1, ll2) = normalizeLLs bo
      (Tile (_, X x1, Y y1)) = lngLatToTile z ll1
      (Tile (_, X x2, Y y2)) = lngLatToTile z ll2
    in [Tile (z, X x, Y y) | x <- [x1..x2], y <- [y1..y2]]
  where normalizeLLs SW = (LngLat (lng1, lat2), LngLat (lng2, lat1))
        normalizeLLs NW = ll

clip :: Z -> Pixel -> Pixel
clip z (Pixel (Px x, Py y)) = Pixel $ bimap (Px . clip' pixels) (Py . clip' pixels) (x, y)
  where clip' mx = min mx . max 0
        pixels = maxPixelsInDimension z

clipBounded :: (Bounded a, Ord a) => a -> a
clipBounded = max minBound . min maxBound
