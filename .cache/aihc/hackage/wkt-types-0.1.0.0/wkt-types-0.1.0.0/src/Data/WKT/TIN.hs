{-# LANGUAGE OverloadedStrings#-}

module Data.WKT.TIN (module Data.WKT.TIN) where

import Data.WKT.Classes
import Data.WKT.Triangle
import Data.List (intercalate, group, sort)
import Data.WKT.Point
import Data.WKT.Helpers (generateZMString)
import Data.Text (pack)

newtype TIN a = TIN [Triangle a]

instance Show a => Show (TIN a) where
    show (TIN triangles) = intercalate ", " triangles'
        where
            triangles' = map (\(Triangle triangle) -> "(" <> intercalate ", " (show <$> triangle) <> ")") triangles

instance Show a => ToWKT (TIN a) where
    toWKT (TIN triangles) = "TIN" <> zmString <> "(" <> pack (show triangles) <> ")"
        where
            (Triangle firstTriangle) = head triangles
            first = head firstTriangle
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Ord a => Valid (TIN a) where
    isValid (TIN triangles) = validTriangles && isContinuous
        where
            validTriangles = all isValid triangles
            allPoints = concatMap (\(Triangle points) -> points) triangles
            isContinuous =  notElem 1 $ length <$> group (sort allPoints) -- checks if every point is on at least two sides.