{-# LANGUAGE RecordWildCards #-}
module Data.Aeson.JSONPath.Query.Query
  ( qQuery
  , qQueryLocated
  )
  where

import Control.Monad                   (join)
import Data.Aeson                      (Value)
import Data.Vector                     (Vector)

import qualified Data.Vector           as V

import Data.Aeson.JSONPath.Types
import Data.Aeson.JSONPath.Query.Segment

import Prelude

-- | Runs the JSONPath Query
qQuery :: Query -> QueryState -> Vector Value
qQuery Query{..} qS@QueryState{..} = case queryType of
  Root    -> foldl applySegment (V.singleton rootVal) querySegments
  Current -> foldl applySegment (V.singleton curVal) querySegments
  where
    applySegment :: Vector Value -> QuerySegment Query -> Vector Value
    applySegment vec seg = join $ V.map (\x -> qQuerySegment seg qS{ curVal = x }) vec

-- | Runs the JSONPath Query also returning node locations
qQueryLocated :: Query -> QueryState -> String -> Vector (String,Value)
qQueryLocated Query{..} qS@QueryState{..} loc = case queryType of
  Root    -> foldl applySegment (V.singleton (loc,rootVal)) querySegments
  Current -> foldl applySegment (V.singleton (loc,curVal)) querySegments
  where
    applySegment :: Vector (String,Value) -> QuerySegment Query -> Vector (String,Value)
    applySegment vec seg = join $ V.map (\(location,cur) -> qQuerySegmentLocated seg qS{ curVal = cur } location) vec
