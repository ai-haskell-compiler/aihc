{-# LANGUAGE DeriveFunctor #-}

module Data.WKT.Primitives (module Data.WKT.Primitives) where

import Data.WKT.Classes
import Data.WKT.Point
import Data.WKT.LineString
import Data.WKT.Polygon
import Data.WKT.Triangle
import Data.Attoparsec.Text (parseOnly, skipSpace, Parser)
import Control.Applicative ((<|>))


data Primitives a = PrimPoint (Point a)| PrimLine (LineString a)| PrimPolygon (Polygon a)| PrimTriangle (Triangle a)
    deriving (Functor, Eq)

instance Show a => Show (Primitives a) where
    show (PrimPoint a)    = show a
    show (PrimLine a)     = show a
    show (PrimPolygon a)  = show a
    show (PrimTriangle a) = show a

instance Show a => ToWKT (Primitives a) where
    toWKT (PrimPoint a)    = toWKT a
    toWKT (PrimLine a)     = toWKT a
    toWKT (PrimPolygon a)  = toWKT a
    toWKT (PrimTriangle a) = toWKT a

instance Eq a => Valid (Primitives a) where
    isValid (PrimPoint a)    = isValid a
    isValid (PrimLine a)     = isValid a
    isValid (PrimPolygon a)  = isValid a
    isValid (PrimTriangle a) = isValid a

instance FromWKT Primitives where
    fromWKT = either (error . show) id . parseOnly wktParser

instance ParseableFromWKT Primitives where
    wktParser = do
        skipSpace
        PrimPoint <$> pointWktParser <|> PrimLine <$> lineStringParser <|> PrimPolygon <$> polygonParser -- <|> PrimTriangle <$> triangleParser
            where
                pointWktParser = wktParser :: Parser (Point Double)
                lineStringParser = wktParser :: Parser (LineString Double)
                polygonParser = wktParser :: Parser (Polygon Double)
                -- triangleParser = wktParser :: Parser (Triangle Double)
