{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE DeriveFunctor#-}
module Data.WKT.MultiPolygon (module Data.WKT.MultiPolygon) where

import Data.WKT.Classes
import Data.WKT.Polygon
import Data.WKT.LineString
import Data.WKT.Point
import Data.WKT.Helpers
import Data.List (intercalate)
import Data.Text (pack, Text)
import Data.Attoparsec.Text (parseOnly, skipSpace, asciiCI, Parser)
import Control.Applicative ((<|>))

newtype MultiPolygon a = MultiPolygon [Polygon a]
    deriving (Functor, Eq)

instance Show a => Show (MultiPolygon a) where
    show (MultiPolygon polygons) = intercalate ", " polygons'
        where
            polygons' =showP <$> polygons

instance Show a => ToWKT (MultiPolygon a) where
    toWKT multiPolygon = "MultiPolygon" <> zmString <> "(" <> pack (show multiPolygon) <> ")"
        where
            (MultiPolygon polygons) = multiPolygon
            (Polygon firstPolygon) = head polygons
            (LineString firstLine) = head firstPolygon
            first = head firstLine
            z' = z first
            m' = m first

            zmString = generateZMString z' m'
instance Eq a => Valid (MultiPolygon a) where
    isValid (MultiPolygon polygons') = all isValid polygons'

instance FromWKT MultiPolygon where
    fromWKT = either (error . show) id . parseOnly wktParser

instance ParseableFromWKT MultiPolygon where
    wktParser = do
        skipSpace
        _ <- asciiCI "MULTIPOLYGON"
        (zFlag, mFlag) <- zmParser
        _ <- "("
        parseMultiPolygon zFlag mFlag

parseMultiPolygon :: Text -> Text -> Parser (MultiPolygon Double)
parseMultiPolygon zFlag mFlag = MultiPolygon <$> polygonParser zFlag mFlag
        where
            polygonParser zFlag' mFlag' = do
                skipSpace
                _ <- "("
                newPolygon <- parsePolygon zFlag' mFlag'
                skipSpace
                closing <- ")" <|> ""
                if closing /= "" then
                    return [newPolygon]
                else do
                        (newPolygon :) <$> ("," *> polygonParser zFlag' mFlag')