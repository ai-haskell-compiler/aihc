module Data.String.Interpolate.Whitespace where

import Data.List ( intercalate )

import Data.String.Interpolate.Types

-- |
-- Collapse all the lines given into a single line, collapsing any whitespace
-- found into a single space and removing begining/trailing whitespace.
collapseWhitespace :: Lines -> Line
collapseWhitespace lines =
  let oneliner = intercalate [Spaces 1] lines
  in removeSurroundingWS $ toSingleSpace oneliner

toSingleSpace :: Line -> Line
toSingleSpace [] = []
toSingleSpace (x:y:xs) | isSpace x && isSpace y =
  toSingleSpace (Spaces 1 : xs)
toSingleSpace (x:xs) | isSpace x =
  Spaces 1 : toSingleSpace xs
toSingleSpace (x:xs) =
  x : toSingleSpace xs

removeSurroundingWS :: Line -> Line
removeSurroundingWS =
    dropWhile isSpace
  . reverse
  . dropWhile isSpace
  . reverse

isSpace :: InterpSegment -> Bool
isSpace (Spaces _) = True
isSpace (Tabs _)   = True
isSpace _other     = False
