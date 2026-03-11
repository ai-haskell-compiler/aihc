{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE DeriveFunctor #-}

module Data.WKT.Triangle (module Data.WKT.Triangle) where
    
import Data.WKT.Classes
import Data.WKT.Point

import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (pack)
import Data.WKT.Helpers (allPairs)

newtype Triangle a  = Triangle [Point a]
    deriving (Functor, Eq)

instance Show a => Show (Triangle a) where
    show (Triangle vertices) = intercalate ", " (show <$> vertices)

instance Show a => ToWKT (Triangle a) where
    toWKT triangle = "Triangle" <> zmString <> "(" <> pack (show triangle) <> ")"
        where
            Triangle vertices = triangle
            first = head vertices
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

instance Eq a => Valid (Triangle a) where
    isValid (Triangle lines') = firstPoint == lastPoint && size == 4
        where
            firstPoint = head  lines'
            lastPoint = head lines'
            size = length lines'

-- Must be valid Triangle
allSides :: Triangle a-> [(Point a, Point a)]
allSides (Triangle vertices) = allPairs vertices