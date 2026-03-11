{-# LANGUAGE OverloadedStrings#-}

module Test.Data.WKT.TestElements (module Test.Data.WKT.TestElements) where

import Data.WKT.Point
import Data.WKT.LineString
import Data.WKT.Polygon
import Data.WKT.MultiPoint
import Data.WKT.MultiLineString
import Data.WKT.MultiPolygon
import Data.WKT.GeometryCollection
import Data.WKT.Primitives
import Data.Text (Text)



-- Primitives
-- Points
point :: Point Int
point = Point {x=1, y=2, z=Nothing, m=Nothing}
pointText :: Text
pointText = "Point (1 2)"

pointZ :: Point Int
pointZ = Point {x=1, y=2, z=Just 3, m=Nothing}
pointZText :: Text
pointZText = "Point Z (1 2 3)"

pointM :: Point Int
pointM = Point {x=1, y=2, z=Nothing, m=Just 4}
pointMText :: Text
pointMText = "Point M (1 2 4)"

pointZM :: Point Int
pointZM = Point {x=1, y=2, z=Just 3, m=Just 4}
pointZMText :: Text
pointZMText = "Point ZM (1 2 3 4)"

-- LineStrings
lineString :: LineString Int
lineString = LineString [point, point]
lineStringText :: Text
lineStringText = "LineString (1 2, 1 2)"

lineStringZ :: LineString Int
lineStringZ = LineString [pointZ, pointZ]
lineStringZText :: Text
lineStringZText = "LineString Z (1 2 3, 1 2 3)"

lineStringM :: LineString Int
lineStringM = LineString [pointM, pointM]
lineStringMText :: Text
lineStringMText = "LineString M (1 2 4, 1 2 4)"

lineStringZM :: LineString Int
lineStringZM = LineString [pointZM, pointZM]
lineStringZMText :: Text
lineStringZMText = "LineString ZM (1 2 3 4, 1 2 3 4)"

-- Triangles (TODO)
-- triangle :: Triangle Int
-- triangle = Triangle [point, point, point, point]

-- triangleZ :: Triangle Int
-- triangleZ = Triangle [pointZ, pointZ, pointZ, pointZ]

-- triangleM :: Triangle Int
-- triangleM = Triangle [pointM, pointM, pointM, pointM]

-- triangleZM :: Triangle Int
-- triangleZM = Triangle [pointZM, pointZM, pointZM, pointZM]

-- Polygons
polygon :: Polygon Int
polygon = Polygon [lineString]
polygonText :: Text
polygonText = "Polygon ((1 2, 1 2))"

polygonZ :: Polygon Int
polygonZ = Polygon [lineStringZ]
polygonZText :: Text
polygonZText = "Polygon Z ((1 2 3, 1 2 3))"

polygonM :: Polygon Int
polygonM = Polygon [lineStringM]
polygonMText :: Text
polygonMText = "Polygon M ((1 2 4, 1 2 4))"

polygonZM :: Polygon Int
polygonZM = Polygon [lineStringZM]
polygonZMText :: Text
polygonZMText = "Polygon ZM ((1 2 3 4, 1 2 3 4))"

polygonR :: Polygon Int
polygonR = Polygon [lineString, lineString]
polygonRText :: Text
polygonRText = "Polygon ((1 2, 1 2), (1 2, 1 2))"

-- Multiparts
-- MultiPoints
multiPoint :: MultiPoint Int
multiPoint = MultiPoint [point, point]
multiPointText :: Text
multiPointText = "MultiPoint ((1 2), (1 2))"

multiPointZ :: MultiPoint Int
multiPointZ = MultiPoint [pointZ, pointZ]
multiPointZText :: Text
multiPointZText = "MultiPoint Z ((1 2 3), (1 2 3))"

multiPointM :: MultiPoint Int
multiPointM = MultiPoint [pointM, pointM]
multiPointMText :: Text
multiPointMText = "MultiPoint M ((1 2 4), (1 2 4))"

multiPointZM :: MultiPoint Int
multiPointZM = MultiPoint [pointZM, pointZM]
multiPointZMText :: Text
multiPointZMText = "MultiPoint ZM ((1 2 3 4), (1 2 3 4))"

-- MultiLineStrings
multiLineString :: MultiLineString Int
multiLineString = MultiLineString [lineString, lineString]
multiLineStringText :: Text
multiLineStringText = "MultiLineString ((1 2, 1 2), (1 2, 1 2))"

multiLineStringZ :: MultiLineString Int
multiLineStringZ = MultiLineString [lineStringZ, lineStringZ]
multiLineStringZText :: Text
multiLineStringZText = "MultiLineString Z ((1 2 3, 1 2 3), (1 2 3, 1 2 3))"

multiLineStringM :: MultiLineString Int
multiLineStringM = MultiLineString [lineStringM, lineStringM]
multiLineStringMText :: Text
multiLineStringMText = "MultiLineString M ((1 2 4, 1 2 4), (1 2 4, 1 2 4))"

multiLineStringZM :: MultiLineString Int
multiLineStringZM = MultiLineString [lineStringZM, lineStringZM]
multiLineStringZMText :: Text
multiLineStringZMText = "MultiLineString ZM ((1 2 3 4, 1 2 3 4), (1 2 3 4, 1 2 3 4))"

-- MultiPolygons
multiPolygon :: MultiPolygon Int
multiPolygon = MultiPolygon [polygon, polygon]
multiPolygonText :: Text
multiPolygonText = "MultiPolygon (((1 2, 1 2)), ((1 2, 1 2)))"

multiPolygonZ :: MultiPolygon Int
multiPolygonZ = MultiPolygon [polygonZ, polygonZ]
multiPolygonZText :: Text
multiPolygonZText = "MultiPolygon Z (((1 2 3, 1 2 3)), ((1 2 3, 1 2 3)))"

multiPolygonM :: MultiPolygon Int
multiPolygonM = MultiPolygon [polygonM, polygonM]
multiPolygonMText :: Text
multiPolygonMText = "MultiPolygon M (((1 2 4, 1 2 4)), ((1 2 4, 1 2 4)))"

multiPolygonZM :: MultiPolygon Int
multiPolygonZM = MultiPolygon [polygonZM, polygonZM]
multiPolygonZMText :: Text
multiPolygonZMText = "MultiPolygon ZM (((1 2 3 4, 1 2 3 4)), ((1 2 3 4, 1 2 3 4)))"

-- PolyhedralSurface (TODO)

-- TIN (TODO)

-- GeometryCollection
geometryCollectionHomo :: GeometryCollection Int
geometryCollectionHomo = GeometryCollection [PrimPoint point, PrimPoint point]
geometryCollectionHomoText :: Text
geometryCollectionHomoText = "GeometryCollection ( Point (1 2), Point (1 2))"


geometryCollectionHetero :: GeometryCollection Int
geometryCollectionHetero = GeometryCollection [PrimPoint point, PrimLine lineString]
geometryCollectionHeteroText :: Text
geometryCollectionHeteroText = "GeometryCollection ( Point (1 2), LineString (1 2, 1 2))"