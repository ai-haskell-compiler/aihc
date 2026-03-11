module Main where

import           Control.Monad
import           Data.Tile
import           Test.HUnit.Base
import           Test.HUnit.Text

main :: IO ()
main = void $ runTestTT $ TestList [
    TestCase convertT000ToPx
  , TestCase convertT000ToLL
  , TestCase convertT110ToPx
  , TestCase convertT110ToLL
  , TestCase llToTile
  , TestCase tileBoundsSW
  , TestCase tileBoundsNW
  , TestCase validateMkTile
  , TestCase childrenOfT110
  , TestCase parentOfT110
  , TestCase parentOfT000
  , TestCase tmsConversion
  , TestCase tilesInBBOX
  ]

convertT000ToPx :: Assertion
convertT000ToPx =
  Pixel (Px 0, Py 0) @=? tileToPixel (Tile (Z 0, X 0, Y 0))

convertT000ToLL :: Assertion
convertT000ToLL =
  LngLat (Lng (-180.0), Lat 85.05112877980659) @=? tileToLngLat (Tile (Z 0, X 0, Y 0))

convertT110ToPx :: Assertion
convertT110ToPx =
  Pixel (Px 256, Py 0) @=? tileToPixel (Tile (Z 1, X 1, Y 0))

convertT110ToLL :: Assertion
convertT110ToLL =
  LngLat (Lng 0.0, Lat 85.05112877980659) @=? tileToLngLat (Tile (Z 1, X 1, Y 0))

llToTile :: Assertion
llToTile =
  Tile (Z 8, X 75, Y 96) @=? lngLatToTile (Z 8) (LngLat (Lng (-74.17250), Lat 40.39187))

tileBoundsSW :: Assertion
tileBoundsSW =
  TileBounds SW (LngLat (Lng (-74.53125), Lat 39.90973623453718))
                (LngLat (Lng (-73.125), Lat 40.97989806962013)) @=?
  tileToBounds SW (Tile (Z 8, X 75, Y 96))

tileBoundsNW :: Assertion
tileBoundsNW =
  TileBounds NW (LngLat (Lng (-74.53125), Lat 40.97989806962013))
                (LngLat (Lng (-73.125), Lat 39.90973623453718)) @=?
  tileToBounds NW (Tile (Z 8, X 75, Y 96))

childrenOfT110 :: Assertion
childrenOfT110 =
  [
    Tile (Z 2, X 2, Y 0)
  , Tile (Z 2, X 2, Y 1)
  , Tile (Z 2, X 3, Y 0)
  , Tile (Z 2, X 3, Y 1)
  ] @=? subTiles (Tile (Z 1, X 1, Y 0))

parentOfT110 :: Assertion
parentOfT110 =
  Just (Tile (Z 0, X 0, Y 0)) @=? parentTile (Tile (Z 1, X 1, Y 0))

parentOfT000 :: Assertion
parentOfT000 =
  Nothing @=? parentTile (Tile (Z 0, X 0, Y 0))

validateMkTile :: Assertion
validateMkTile =
  Nothing @=? mkTile (Z 0) (X 1) (Y 1)

tmsConversion :: Assertion
tmsConversion =
  Tile (Z 8, X 75, Y 159) @=? flipY (Tile (Z 8, X 75, Y 96))

tilesInBBOX :: Assertion
tilesInBBOX =
  [
    Tile (Z 15, X 9636, Y 12332)
  , Tile (Z 15, X 9636, Y 12333)
  , Tile (Z 15, X 9636, Y 12334)
  , Tile (Z 15, X 9636, Y 12335)
  , Tile (Z 15, X 9637, Y 12332)
  , Tile (Z 15, X 9637, Y 12333)
  , Tile (Z 15, X 9637, Y 12334)
  , Tile (Z 15, X 9637, Y 12335)
  , Tile (Z 15, X 9638, Y 12332)
  , Tile (Z 15, X 9638, Y 12333)
  , Tile (Z 15, X 9638, Y 12334)
  , Tile (Z 15, X 9638, Y 12335)
  , Tile (Z 15, X 9639, Y 12332)
  , Tile (Z 15, X 9639, Y 12333)
  , Tile (Z 15, X 9639, Y 12334)
  , Tile (Z 15, X 9639, Y 12335)
  , Tile (Z 15, X 9640, Y 12332)
  , Tile (Z 15, X 9640, Y 12333)
  , Tile (Z 15, X 9640, Y 12334)
  , Tile (Z 15, X 9640, Y 12335)
  ] @=? tilesInBounds NW (Z 15) (ll1, ll2)
  where ll1 = LngLat (Lng (-74.13451064965257), Lat 40.60668448525735)
        ll2 = LngLat (Lng (-74.08404221265933), Lat 40.58875358944036)

