module Test.Data.WKT.FromWKT (module Test.Data.WKT.FromWKT) where
    
import Test.Hspec

import Data.WKT.Classes

import Test.Data.WKT.TestElements
import Data.WKT.Point (Point(..))
import Data.WKT.LineString (LineString(..))
import Data.WKT.Polygon (Polygon(..))
import Data.WKT.MultiPoint (MultiPoint(..))
import Data.WKT.MultiLineString (MultiLineString(..))
import Data.WKT.MultiPolygon (MultiPolygon(..))


fromWktTests :: Spec
fromWktTests = do
    points
    lineStrings
    polygons
    multiPoints
    multiLineStrings
    multiPolygons
    -- geometryCollections

points :: Spec
points = do
    describe "FromWKT Points" $ do
        it "should convert a WKT Text to a 2D Point" $ do
            floor <$> (fromWKT pointText :: Point Double)  `shouldBe` point
        it "should convert a WKT Text to a 3D (Z) Point " $ do
            floor <$> (fromWKT pointZText :: Point Double)  `shouldBe` pointZ
        it "should convert a WKT Text to a 3D (M) Point" $ do
            floor <$> (fromWKT pointMText :: Point Double)  `shouldBe` pointM
        it "should convert a WKT Text to a 4D Point" $ do
            floor <$> (fromWKT pointZMText :: Point Double) `shouldBe` pointZM

lineStrings :: Spec
lineStrings = do
    describe "FromWKT LineStrings" $ do
        it "should convert a WKT Text to a 2D LineString" $ do
            floor <$> (fromWKT lineStringText :: LineString Double)  `shouldBe` lineString
        it "should convert a WKT Text to a 3D (Z) LineString " $ do
            floor <$> (fromWKT lineStringZText :: LineString Double)  `shouldBe` lineStringZ
        it "should convert a WKT Text to a 3D (M) LineString" $ do
            floor <$> (fromWKT lineStringMText :: LineString Double)  `shouldBe` lineStringM
        it "should convert a WKT Text to a 4D LineString" $ do
            floor <$> (fromWKT lineStringZMText :: LineString Double) `shouldBe` lineStringZM
        
polygons :: Spec
polygons = do
    describe "FromWKT Polygons" $ do
        it "should convert a WKT Text to a 2D Polygon" $ do
            floor <$> (fromWKT polygonText :: Polygon Double)  `shouldBe` polygon
        it "should convert a WKT Text to a 3D (Z) Polygon " $ do
            floor <$> (fromWKT polygonZText :: Polygon Double)  `shouldBe` polygonZ
        it "should convert a WKT Text to a 3D (M) Polygon" $ do
            floor <$> (fromWKT polygonMText :: Polygon Double)  `shouldBe` polygonM
        it "should convert a WKT Text to a 4D Polygon" $ do
            floor <$> (fromWKT polygonZMText :: Polygon Double) `shouldBe` polygonZM
        it "should convert a WKT Text to Polygon with two rings" $ do
            floor <$> (fromWKT polygonRText :: Polygon Double) `shouldBe` polygonR

multiPoints :: Spec
multiPoints = do
    describe "FromWKT MultiPoints" $ do
        it "should convert a WKT Text to a 2D MultiPoint" $ do
            floor <$> (fromWKT multiPointText :: MultiPoint Double)  `shouldBe` multiPoint
        it "should convert a WKT Text to a 3D (Z) MultiPoint " $ do
            floor <$> (fromWKT multiPointZText :: MultiPoint Double)  `shouldBe` multiPointZ
        it "should convert a WKT Text to a 3D (M) MultiPoint" $ do
            floor <$> (fromWKT multiPointMText :: MultiPoint Double)  `shouldBe` multiPointM
        it "should convert a WKT Text to a 4D MultiPoint" $ do
            floor <$> (fromWKT multiPointZMText :: MultiPoint Double) `shouldBe` multiPointZM

multiLineStrings :: Spec
multiLineStrings = do
    describe "FromWKT MultiLineStrings" $ do
        it "should convert a WKT Text to a 2D MultiLineString" $ do
            floor <$> (fromWKT multiLineStringText :: MultiLineString Double)  `shouldBe` multiLineString
        it "should convert a WKT Text to a 3D (Z) MultiLineString " $ do
            floor <$> (fromWKT multiLineStringZText :: MultiLineString Double)  `shouldBe` multiLineStringZ
        it "should convert a WKT Text to a 3D (M) MultiLineString" $ do
            floor <$> (fromWKT multiLineStringMText :: MultiLineString Double)  `shouldBe` multiLineStringM
        it "should convert a WKT Text to a 4D MultiLineString" $ do
            floor <$> (fromWKT multiLineStringZText :: MultiLineString Double) `shouldBe` multiLineStringZ

multiPolygons :: Spec
multiPolygons = do
    describe "FromWKT MultiPolygons" $ do
        it "should convert a WKT Text to a 2D MultiPolygon" $ do
            floor <$> (fromWKT multiPolygonText :: MultiPolygon Double)  `shouldBe` multiPolygon
        it "should convert a WKT Text to a 3D (Z) MultiPolygon " $ do
            floor <$> (fromWKT multiPolygonZText :: MultiPolygon Double)  `shouldBe` multiPolygonZ
        it "should convert a WKT Text to a 3D (M) MultiPolygon" $ do
            floor <$> (fromWKT multiPolygonMText :: MultiPolygon Double)  `shouldBe` multiPolygonM
        it "should convert a WKT Text to a 4D MultiPolygon" $ do
            floor <$> (fromWKT multiPolygonZMText :: MultiPolygon Double) `shouldBe` multiPolygonZM