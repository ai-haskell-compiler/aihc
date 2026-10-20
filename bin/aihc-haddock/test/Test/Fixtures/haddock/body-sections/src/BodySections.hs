-- | A module without an export list. The section headings in the body give
-- the structure of the documentation.
module BodySections where

-- * Shapes

-- | A shape.
data Shape = Circle Double | Square Double

-- ** Areas

-- | The area of a shape.
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Square s) = s * s

-- | Double an area.
twice :: Double -> Double
twice x = x + x

-- * Colours

-- | A colour.
data Colour = Red | Green
