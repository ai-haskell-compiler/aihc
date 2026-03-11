{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE InstanceSigs #-}

module Data.WKT.Geometries (module Data.WKT.Geometries) where

import Data.WKT.Point (Point(..))
import Data.WKT.LineString (LineString(..))
import Data.WKT.Polygon (Polygon(..))
import Data.WKT.MultiPoint (MultiPoint(..))
import Data.WKT.MultiLineString (MultiLineString(..))
import Data.WKT.MultiPolygon (MultiPolygon(..))
import Data.WKT.GeometryCollection (GeometryCollection(..))
import Data.WKT.Classes
import Data.Attoparsec.Text (Parser, atEnd, parseOnly, skipSpace)
import Control.Applicative ((<|>))


-- | All WKT geometries.
data Geometries a = Geometries{
    points              :: [Point a],
    lineStrings         :: [LineString a],
    polygons            :: [Polygon a],
    -- triangles           :: [Triangle a]
    multiPoints         :: [MultiPoint a],
    multiLineString     :: [MultiLineString a],
    multiPolygon        :: [MultiPolygon a],
    -- tins                :: [TINs a]
    geometryCollections :: [GeometryCollection a]
}  
    deriving (Functor)

instance Semigroup (Geometries a) where
    geometry1 <> geometry2 = Geometries{
        points = points geometry1 <> points geometry2,
        lineStrings = lineStrings geometry1 <> lineStrings geometry2,
        polygons = polygons geometry1 <> polygons geometry2,
        -- triangles = triangles geometry1 <> triangles geometry2,
        multiPoints = multiPoints geometry1 <> multiPoints geometry2,
        multiLineString = multiLineString geometry1 <> multiLineString geometry2,
        multiPolygon = multiPolygon geometry1 <> multiPolygon geometry2,
        -- tins = tins geometry1 <> tins geometry2,
        geometryCollections = geometryCollections geometry1 <> geometryCollections geometry2
    }

instance Monoid (Geometries a) where
    mempty = emptyGeometry

emptyGeometry :: Geometries a
emptyGeometry = Geometries [] [] [] [] [] [] []

instance Show a => Show (Geometries a) where
    show geometry = 
        "points: " <> show (points geometry) <> "\n" <>
        "lineStrings: " <> show (lineStrings geometry) <> "\n" <>
        "polygons: " <> show (polygons geometry) <> "\n" <>
        -- "triangles: " <> show (triangles geometry) <> "\n" <>
        "multiPoints: " <> show (multiPoints geometry) <> "\n" <>
        "multiLineString: " <> show (multiLineString geometry) <> "\n" <>
        "multiPolygon: " <> show (multiPolygon geometry) <> "\n" <>
        -- "TINs: " <> show (tins geometry) <> "\n" <>
        "geometryCollections: " <> show (geometryCollections geometry) <> "\n"

instance Show a => ToWKT (Geometries a) where
    toWKT geometry = 
        foldl (<>) "" (map ((<>"\n").toWKT) (points geometry)) <>
        foldl (<>) "" (map ((<>"\n").toWKT) (lineStrings geometry)) <>
        foldl (<>) "" (map ((<>"\n").toWKT) (polygons geometry)) <>
        -- foldl (<>) "" (map ((<>"\n").toWKT) (triangles geometry)) <>
        foldl (<>) "" (map ((<>"\n").toWKT) (multiPoints geometry)) <>
        foldl (<>) "" (map ((<>"\n").toWKT) (multiLineString geometry)) <>
        foldl (<>) "" (map ((<>"\n").toWKT) (multiPolygon geometry)) <>
        -- foldl (<>) "" (map ((<>"\n").toWKT) (tins geometry)) <>
        foldl (<>) "" (map ((<>"\n").toWKT) (geometryCollections geometry))

instance Eq a => Valid (Geometries a) where
    isValid geometry = 
        all isValid (points geometry) &&
        all isValid (lineStrings geometry) &&
        all isValid (polygons geometry) &&
        -- all isValid (triangles geometry) &&
        all isValid (multiPoints geometry) &&
        all isValid (multiLineString geometry) &&
        all isValid (multiPolygon geometry) &&
        -- all isValid (tins geometry) &&
        all isValid (geometryCollections geometry)

instance FromWKT Geometries where
    fromWKT = either (error . show) id . parseOnly wktParser

instance ParseableFromWKT Geometries where
    wktParser :: Parser (Geometries Double)
    wktParser = do
        skipSpace
        atEnd' <- atEnd
        if atEnd' then return emptyGeometry else do
            parsedValue <-  pointParser <|> 
                            lineStringParser  <|> 
                            polygonParser <|> 
                            multiPointParser <|> 
                            multiLineStringParser <|> 
                            multiPolygonParser <|> 
                            geometryCollectionParser
            (<> parsedValue) <$> wktParser

        where
            pointParser = (\p -> emptyGeometry{points = [p]}) <$> (wktParser :: Parser (Point Double))
            lineStringParser = (\l -> emptyGeometry{lineStrings = [l]}) <$> (wktParser :: Parser (LineString Double))
            polygonParser = (\p -> emptyGeometry{polygons = [p]}) <$> (wktParser :: Parser (Polygon Double))
            multiPointParser = (\mp -> emptyGeometry{multiPoints = [mp]}) <$> (wktParser :: Parser (MultiPoint Double))
            multiLineStringParser = (\ml -> emptyGeometry{multiLineString = [ml]}) <$> (wktParser :: Parser (MultiLineString Double))
            multiPolygonParser = (\mp -> emptyGeometry{multiPolygon = [mp]}) <$> (wktParser :: Parser (MultiPolygon Double))
            geometryCollectionParser = (\gc -> emptyGeometry{geometryCollections = [gc]}) <$> (wktParser :: Parser (GeometryCollection Double))