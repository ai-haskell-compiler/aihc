# tile

Haskell library for interfacing with [slippy map tiles](http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames) and converting between `Pixel`, `Tile`, and `LngLat` types.

## Basic Usage

Converting between the three main types, `Pixel`, `Tile`, and `LngLat` is a major piece of this library's functionality.

### Convert `LngLat` to `Tile`

Convert a longitude and latitude pair at a given
zoom level into a tile.

```haskell

lngLatToTile (Z 8) (LngLat (Lng (-74.17250), Lat 40.39187))
-- returns Tile (Z 8, X 75, Y 96)

```

### Convert `LngLat` to `Pixel`

Convert a longitude and latitude pair at a given
zoom level into a pixel on the map.

```haskell

lngLatToPixel (Z 8) (LngLat (Lng (-74.17250), Lat 40.39187))
-- returns Pixel (Px 19265,Py 24717)

```

### Convert `Tile` to `LngLat`

Convert a tile back to a longitude and latitude pair.

```haskell

tileToLngLat (Tile (Z 1, X 1, Y 0))
-- returns LngLat (Lng 0.0, Lat 85.05112877980659)

```

### Convert `Tile` to `Pixel`

Convert a tile into a pixel on the map.

```haskell

tileToPixel (Tile (Z 1, X 1, Y 0))
-- returns Pixel (Px 256,Py 0)

```

### Convert `Pixel` to `Tile`

Convert a pixel to a tile.

```haskell

pixelToTile (Z 1) $ Pixel (Px 256,Py 0)
-- returns Tile (Z 1,X 1,Y 0)

```

### Convert `Pixel` to `LngLat`

Convert a pixel into a longitude and latitude pair.

```haskell

pixelToLngLat (Z 8) $ Pixel (Px 19265,Py 24717)
-- returns LngLat (Lng (-74.1741943359375),Lat 40.39258071969131)

```

### Subtiles

Get the children of a given tile.

```haskell

subTiles (Tile (Z 1, X 1, Y 0))
-- returns [
--    Tile (Z 2, X 2, Y 0)
--  , Tile (Z 2, X 2, Y 1)
--  , Tile (Z 2, X 3, Y 0)
--  , Tile (Z 2, X 3, Y 1)
--  ]

```

### Parent tiles

Get the parent of a given tile.

```haskell

parentTile (Tile (Z 1, X 1, Y 0))
-- returns Just (Tile (Z 0, X 0, Y 0))

```

### Tile Bounds

Convert a tile into a bounding box.

#### Oriented from the SW

``` haskell

tileToBounds SW (Tile (Z 8, X 75, Y 96))
-- returns TileBounds SW (LngLat (Lng (-74.53125), Lat 39.90973623453718))
--                       (LngLat (Lng (-73.125), Lat 40.97989806962013))

```

#### Oriented from the NW

``` haskell

tileToBounds NW (Tile (Z 8, X 75, Y 96))
-- returns TileBounds SW (LngLat (Lng (-74.53125), Lat 40.97989806962013))
--                       (LngLat (Lng (-73.125), Lat 39.90973623453718))

```

## Contributing

Contributions are welcome!