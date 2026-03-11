module HSlippyMap (
  Tile (..),
  Lat, Long,
  X, Y, ZLevel,
  tilesFromBBox,
  tileFromLatLong,
  tileFromXY
) where

type Lat = Float
type Long = Float
type X = Integer
type Y = Integer
type ZLevel = Integer

data Tile = Tile {
  tlat :: Lat,
  tlong :: Long,
  tx :: X,
  ty :: Y,
  tz :: ZLevel }

instance Show Tile where
  show (Tile lat long x y z) = "http://tile.openstreetmap.org/" ++ show z ++ "/" ++ show x ++ "/" ++ show y ++ ".png"

-- if not same z-level == Nothing
tilesFromBBox :: Tile -> Tile -> Maybe [Tile]
tilesFromBBox min max |  (tz min) == (tz max) = Just $ concat $ map (\(x,y) -> map (\y'-> Tile (tlat min) (tlong min) x y' z) y)
                         [(x,[(minimum [tymin, tymax])..(maximum [tymin,tymax])]) | x <- [(minimum [txmin, txmax])..(maximum [txmin, txmax])]]
                      | otherwise = Nothing
  where
    txmax = tx max
    txmin = tx min
    tymax = ty max
    tymin = ty min
    z = tz min

tileFromLatLong :: Lat -> Long -> ZLevel -> Tile
tileFromLatLong lat lon z = Tile lat lon x y z
    where
        x = long2tilex lon z
        y = lat2tiley lat z

tileFromXY :: X -> Y -> ZLevel -> Tile
tileFromXY x y z = Tile lat lon x y z
    where
        lat = tilex2long x z
        lon = tiley2lat y z

long2tilex lon z = floor((lon + 180) / 360 * (2** fromInteger z::Long))

lat2tiley lat z = floor((1.0 - log( tan(lat * pi/180.0) + 1.0 / cos(lat * pi/180.0)) / pi) / 2.0 * (2 ** fromInteger z::Long))

tilex2long x z = (fromInteger x::Long) / (2.0 ** fromInteger z::Long) * 360.0 - 180

tiley2lat y z = 180.0 / pi * atan(0.5 * (exp n - exp(-n)))
    where
        n = pi - 2.0 * pi * (fromInteger y::Long) / (2.0 ** fromInteger z::Long)
