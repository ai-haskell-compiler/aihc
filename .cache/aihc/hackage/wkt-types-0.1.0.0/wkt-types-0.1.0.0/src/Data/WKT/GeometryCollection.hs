{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE DeriveFunctor #-}

module Data.WKT.GeometryCollection (module Data.WKT.GeometryCollection) where

import Data.WKT.Classes
import Data.WKT.Primitives
import Data.WKT.LineString
import Data.WKT.Triangle
import Data.WKT.Polygon
import Data.WKT.Point
import Data.WKT.Helpers (generateZMString)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Attoparsec.Text (parseOnly, Parser, skipSpace)
import Control.Applicative ((<|>))

newtype GeometryCollection a =  GeometryCollection [Primitives a]
    deriving (Functor, Eq)

instance Show a => Show (GeometryCollection a) where
    show (GeometryCollection collection) = intercalate ", " (show <$> collection)

instance Show a => ToWKT (GeometryCollection a) where
    toWKT (GeometryCollection collection) = "GeometryCollection" <> zmString <> "( " <> T.intercalate ", " (toWKT <$> collection) <> ")"
        where
            first = case head collection of
                PrimPoint a                 -> a
                PrimLine (LineString a)     -> head a
                PrimTriangle (Triangle a)   -> head a
                PrimPolygon (Polygon a)     -> (\(LineString a') -> head a') $ head a
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Eq a => Valid (GeometryCollection a) where
    isValid (GeometryCollection collection') = all isValid collection'

instance FromWKT GeometryCollection where
    fromWKT = either (error . show) id . parseOnly wktParser

instance ParseableFromWKT GeometryCollection where
    wktParser = do
        _ <- "GEOMETRYCOLLECTION"
        -- (zFlag, mFlag) <- zmParser -- unsure? is ZM an internal or external flag?
        _ <- "("
        GeometryCollection <$> primitivesParser
            where
                primitivesParser = do
                    skipSpace
                    primitive <- wktParser :: Parser (Primitives Double)
                    skipSpace
                    closing <- ")" <|> ""
                    if closing /= "" then
                        return [primitive]
                    else do
                        (primitive :) <$> ("," *> primitivesParser)