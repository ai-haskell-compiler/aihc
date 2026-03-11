{-# LANGUAGE RecordWildCards #-}
module Data.Aeson.JSONPath.Query.Segment
  ( qQuerySegment
  , qQuerySegmentLocated
  )
  where

import Control.Monad (join)
import Data.Aeson    (Value)
import Data.Vector   (Vector)

import qualified Data.Aeson         as JSON
import qualified Data.Aeson.KeyMap  as KM
import qualified Data.Aeson.Key     as K
import qualified Data.Vector        as V

import Data.Aeson.JSONPath.Types
import Data.Aeson.JSONPath.Query.Selector

import Prelude

qQuerySegment :: QuerySegment Query -> QueryState -> Vector Value
qQuerySegment QuerySegment{..} qS@QueryState{..} = case segmentType of
  Child      -> joinAfterMap $ V.singleton curVal
  Descendant -> joinAfterMap $ allElemsRecursive curVal
  where
    joinAfterMap vec = join $ V.map (\cur -> qSegment segment qS{ curVal = cur }) vec

qQuerySegmentLocated :: QuerySegment Query -> QueryState-> String -> Vector (String, Value)
qQuerySegmentLocated QuerySegment{..} qS@QueryState{..} loc = case segmentType of
  Child      -> joinAfterMap $ V.singleton (loc,curVal)
  Descendant -> joinAfterMap $ allElemsRecursiveLocated (loc,curVal)
  where
    joinAfterMap vec = join $ V.map (\(location,cur) -> qSegmentLocated segment qS{ curVal = cur } location) vec

qSegment :: Segment Query -> QueryState -> Vector Value
qSegment (Bracketed sels) qS = V.concat $ map (`qSelector` qS) sels
qSegment (Dotted key) qS = qSelector (Name key) qS
qSegment WildcardSegment qS = qSelector WildcardSelector qS

qSegmentLocated :: Segment Query -> QueryState -> String -> Vector (String,Value)
qSegmentLocated (Bracketed sels) qS loc = V.concat $ map (\sel -> qSelectorLocated sel qS loc) sels
qSegmentLocated (Dotted key) qS loc = qSelectorLocated (Name key) qS loc
qSegmentLocated WildcardSegment qS loc = qSelectorLocated WildcardSelector qS loc

-- TODO: Looks kinda ugly, make it pretty <3
allElemsRecursive :: Value -> Vector Value
allElemsRecursive o@(JSON.Object obj) = V.concat [
    V.singleton o,
    V.concat $ map allElemsRecursive (KM.elems obj)
  ]
allElemsRecursive a@(JSON.Array arr) = V.concat [
    V.singleton a,
    V.concat $ map allElemsRecursive (V.toList arr)
  ]
allElemsRecursive _ = V.empty

-- TODO: Looks kinda ugly, make it pretty <3
allElemsRecursiveLocated :: (String,Value) -> Vector (String,Value)
allElemsRecursiveLocated (loc, o@(JSON.Object obj)) = V.concat [
    V.singleton (loc,o),
    V.concat $ zipWith (curry allElemsRecursiveLocated) keys (KM.elems obj)
  ]
  where
    keys = map (toPathKey loc) $ KM.keys obj
allElemsRecursiveLocated (loc, a@(JSON.Array arr)) = V.concat [
    V.singleton (loc,a),
    V.concat $ zipWith (curry allElemsRecursiveLocated) indices (V.toList arr)
  ]
  where
    indices = map (toPathIdx loc) [0..V.length arr - 1]
allElemsRecursiveLocated _ = V.empty

toPathKey :: String -> KM.Key -> String
toPathKey loc key = loc ++ "['" ++ escapeEscapees (K.toString key) ++ "']"
  where
    escapeEscapees :: String -> String
    escapeEscapees [] = []
    escapeEscapees (x:xs) = checkChar x ++ escapeEscapees xs
      where
        -- TODO: Do we need to escape unicode chars?
        checkChar '\\' = ['\\', '\\']
        checkChar '\'' = ['\\', '\'']
        checkChar '\b' = ['\\', 'b']
        checkChar '\r' = ['\\', 'r']
        checkChar '\t' = ['\\', 't']
        checkChar '\f' = ['\\', 'f']
        checkChar '\n' = ['\\', 'n']
        checkChar c = [c]

toPathIdx :: String -> Int -> String
toPathIdx loc idx = loc ++ "[" ++ show idx ++ "]"
