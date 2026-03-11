{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE DeriveFunctor #-}

module Data.WKT.Polygon (module Data.WKT.Polygon) where

import Data.WKT.Classes
import Data.WKT.Point
import Data.WKT.LineString

import Data.List (intercalate)
import Data.WKT.Helpers (generateZMString, zmParser)
import Data.Text (pack, Text)
import Data.Attoparsec.Text (parseOnly, skipSpace, asciiCI, Parser)
import Control.Applicative ((<|>))

newtype Polygon a = Polygon [LineString a]
    deriving (Functor, Eq)

instance Show a => Show (Polygon a) where
    show (Polygon polygon) = intercalate ", " rings
        where
            rings = map (\(LineString ring) -> "(" <> intercalate ", " (show <$> ring) <> ")") polygon

instance Show a => ToWKT (Polygon a) where
    toWKT polygon = "Polygon" <> zmString <> "(" <> pack (show polygon) <> ")"
        where
            Polygon rings = polygon
            (LineString firstLine) = head rings
            first = head firstLine
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Eq a => Valid (Polygon a) where
    isValid (Polygon lines') = validLines && validPolygon
        where
            validLines = all isValid lines'
            validPolygon = all (\(LineString line) -> head line == last line) lines'

instance FromWKT Polygon where
    fromWKT = either (error . show) id . parseOnly wktParser

instance ParseableFromWKT Polygon where
    wktParser = do
        skipSpace
        _ <- asciiCI "POLYGON"
        (zFlag, mFlag) <- zmParser
        _ <- "("
        parsePolygon zFlag mFlag
        
parsePolygon :: Text -> Text -> Parser (Polygon Double)
parsePolygon zFlag mFlag = Polygon <$> ringsParser zFlag mFlag
    where
        ringsParser zFlag' mFlag' = do
            skipSpace
            _ <- "("
            newRing <- parseLineString zFlag' mFlag'
            skipSpace
            closing <- ")" <|> ""
            if closing /= "" then
                return [newRing]
            else do
                    (newRing :) <$> ("," *> ringsParser zFlag' mFlag')