{-# LANGUAGE OverloadedStrings#-}

module Test.Data.WKT.ToWKT (module Test.Data.WKT.ToWKT) where
    
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Data.WKT.TestElements

import Data.WKT.Classes

toWktTests :: Spec
toWktTests = do
    points
    lineStrings
    polygons
    multiPoints
    multiLineStrings
    multiPolygons
    geometryCollections

-- Primitives
points :: Spec
points = do
    describe "ToWKT Points" $ do
        it "should convert a 2D Point to WKT" $ do
            toWKT point `shouldBe` pointText
        it "should convert a 3D (Z) Point to WKT" $ do
            toWKT pointZ `shouldBe` pointZText
        it "should convert a 3D (M) Point to WKT" $ do
            toWKT pointM `shouldBe` pointMText
        it "should convert a 4D Point to WKT" $ do
            toWKT pointZM `shouldBe` pointZMText
    -- TODO: doubles, with e-notation

lineStrings :: Spec
lineStrings = do
    describe "ToWKT LineStrings" $ do
        it "should convert a 2D LineString to WKT" $ do
            toWKT lineString   `shouldBe` lineStringText
        it "should convert a 3D (Z) LineString to WKT" $ do
            toWKT lineStringZ  `shouldBe` lineStringZText
        it "should convert a 3D (M) LineString to WKT" $ do
            toWKT lineStringM  `shouldBe` lineStringMText
        it "should convert a 4D LineString to WKT" $ do
            toWKT lineStringZM `shouldBe` lineStringZMText

polygons :: Spec
polygons = do
    describe "ToWKT Polygons" $ do
        it "should convert a 2D Polygon to WKT" $ do
            toWKT polygon  `shouldBe` polygonText
        it "should convert a 3D (Z) Polygon to WKT" $ do
            toWKT polygonZ  `shouldBe` polygonZText
        it "should convert a 3D (M) Polygon to WKT" $ do
            toWKT polygonM  `shouldBe` polygonMText
        it "should convert a 4D Polygon to WKT" $ do
            toWKT polygonZM `shouldBe` polygonZMText
        it "should convert a 2D Polygon with a hole to WKT" $ do
            toWKT polygonR  `shouldBe` polygonRText

-- Multipart
multiPoints :: Spec
multiPoints = do 
    describe "ToWKT MultiPoints" $ do
        it "should convert a 2D MultiPoint to WKT" $ do
            toWKT multiPoint   `shouldBe` multiPointText
        it "should convert a 3D (Z) MultiPoint to WKT" $ do
            toWKT multiPointZ  `shouldBe` multiPointZText
        it "should convert a 3D (M) MultiPoint to WKT" $ do
            toWKT multiPointM  `shouldBe` multiPointMText
        it "should convert a 4D MultiPoint to WKT" $ do
            toWKT multiPointZM `shouldBe` multiPointZMText

multiLineStrings :: Spec
multiLineStrings = do
    describe "ToWKT MultiLineStrings" $ do
        it "should convert a 2D MultiLineString to WKT" $ do
            toWKT multiLineString   `shouldBe` multiLineStringText
        it "should convert a 3D (Z) MultiLineString to WKT" $ do
            toWKT multiLineStringZ  `shouldBe` multiLineStringZText
        it "should convert a 3D (M) MultiLineString to WKT" $ do
            toWKT multiLineStringM  `shouldBe` multiLineStringMText
        it "should convert a 4D MultiLineString to WKT" $ do
            toWKT multiLineStringZM `shouldBe` multiLineStringZMText

multiPolygons :: Spec
multiPolygons = do
    describe "ToWKT MultiPolygons" $ do
        it "should convert a 2D MultiPolygon to WKT" $ do
            toWKT multiPolygon   `shouldBe` multiPolygonText
        it "should convert a 3D (Z) MultiPolygon to WKT" $ do
            toWKT multiPolygonZ  `shouldBe` multiPolygonZText
        it "should convert a 3D (M) MultiPolygon to WKT" $ do
            toWKT multiPolygonM  `shouldBe` multiPolygonMText
        it "should convert a 4D MultiPolygon to WKT" $ do
            toWKT multiPolygonZM `shouldBe` multiPolygonZMText

geometryCollections :: Spec
geometryCollections = do
    describe "ToWKT GeometryCollections" $ do
        it "should convert a homogenous GeometryCollection to WKT" $ do
            toWKT geometryCollectionHomo   `shouldBe` geometryCollectionHomoText
        it "should convert a heterogenous GeometryCollection to WKT" $ do
            toWKT geometryCollectionHetero `shouldBe` geometryCollectionHeteroText