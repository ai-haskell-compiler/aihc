{-# LANGUAGE RecordWildCards #-}
module Data.Aeson.JSONPath.Query.Selector
  ( qSelector
  , qSelectorLocated
  )
  where

import Data.Aeson     (Value)
import Data.Vector    (Vector)
import Data.Text      (Text)

import qualified Data.Aeson            as JSON
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.Aeson.Key        as K
import qualified Data.Text             as T
import qualified Data.Vector           as V

import Data.Aeson.JSONPath.Types
import Data.Aeson.JSONPath.Query.Filter

import Prelude

qSelector :: Selector Query -> QueryState -> Vector Value
qSelector (Name key) QueryState{curVal=(JSON.Object obj)} = maybe V.empty V.singleton $ KM.lookup (K.fromText key) obj
qSelector (Name _) _ = V.empty
qSelector (Index idx) QueryState{curVal=(JSON.Array arr)} = maybe V.empty V.singleton $ if idx >= 0 then (V.!?) arr idx else (V.!?) arr (idx + V.length arr)
qSelector (Index _) _ = V.empty
qSelector (ArraySlice startEndStep) QueryState{curVal=(JSON.Array arr) } = sliceArray startEndStep arr
qSelector (ArraySlice _) _ = V.empty
qSelector (Filter orExpr) qS = filterOrExpr orExpr qS
qSelector WildcardSelector QueryState{..} = case curVal of
    (JSON.Object obj) -> V.fromList $ KM.elems obj
    (JSON.Array arr)  -> arr
    _                 -> V.empty


qSelectorLocated :: Selector Query -> QueryState -> String -> Vector (String,Value)
qSelectorLocated (Name key) QueryState{curVal=(JSON.Object obj)} loc = maybe V.empty (\x-> V.singleton (toPathKey loc key, x)) $ KM.lookup (K.fromText key) obj
qSelectorLocated (Name _) _ _ = V.empty
qSelectorLocated (Index idx) QueryState{curVal=(JSON.Array arr)} loc = maybe V.empty (\x-> V.singleton (newLocation, x)) $ (V.!?) arr (getIndex idx)
  where
    newLocation = loc ++ "[" ++ show (getIndex idx) ++ "]"
    getIndex i = if i >= 0 then i else i + V.length arr
qSelectorLocated (Index _) _ _ = V.empty
qSelectorLocated (ArraySlice (start,end,step)) QueryState{curVal=(JSON.Array arr)} loc = sliceArrayLocated (start,end,step) $ V.zip (V.fromList locs) arr
  where
    locs = [ loc ++ "[" ++ show i ++ "]" | i <- indices ]
    indices = [0..(V.length arr - 1)]
qSelectorLocated (ArraySlice _) _ _ = V.empty
qSelectorLocated (Filter orExpr) qS loc = filterOrExprLocated orExpr qS loc
qSelectorLocated WildcardSelector QueryState{..} loc = case curVal of
    (JSON.Object obj) -> V.fromList $ zip (locsWithKeys obj) (KM.elems obj)
    (JSON.Array arr)  -> V.zip (V.fromList (locsWithIdxs arr)) arr
    _                 -> V.empty
    where
      locsWithKeys obj = map (toPathKey loc . K.toText) (KM.keys obj)
      locsWithIdxs arr = map (\x -> loc ++ "[" ++ show x ++ "]") [0..(V.length arr)]


sliceArray :: (Maybe Int, Maybe Int, Int) -> Vector Value -> Vector Value
sliceArray (start,end,step) vec =
  case compare step 0 of
    GT -> getSliceForward (maybe 0 normalize start) (maybe len normalize end) step vec
    LT -> getSliceReverse (maybe (len-1) normalize start) (maybe (-1) normalize end) step vec
    EQ -> V.empty
    where
      -- TODO: Looks kinda ugly, make it pretty <3
      len = V.length vec
      normalize i = if i >= 0 then i else len + i

      getSliceForward st en stp arr = loop lower V.empty
        where
          (lower,upper) = (min (max st 0) len, min (max en 0) len)

          loop i acc =
            if i < upper
              then loop (i+stp) $ V.snoc acc $ (V.!) arr (normalize i)
            else
              acc

      getSliceReverse st en stp arr = loop upper V.empty
        where
          (lower,upper) = (min (max en (-1)) (len-1), min (max st (-1)) (len-1))

          loop i acc =
            if lower < i
              then loop (i+stp) $ V.snoc acc $ (V.!) arr (normalize i)
            else
              acc


sliceArrayLocated :: (Maybe Int, Maybe Int, Int) -> Vector (String,Value) -> Vector (String,Value)
sliceArrayLocated (start,end,step) vec =
  case compare step 0 of
    GT -> getSliceForward (maybe 0 normalize start) (maybe len normalize end) step vec
    LT -> getSliceReverse (maybe (len-1) normalize start) (maybe (-1) normalize end) step vec
    EQ -> V.empty
    where
      -- TODO: Looks kinda ugly, make it pretty <3
      len = V.length vec
      normalize i = if i >= 0 then i else len + i

      getSliceForward st en stp arr = loop lower V.empty
        where
          (lower,upper) = (min (max st 0) len, min (max en 0) len)

          loop i acc =
            if i < upper
              then loop (i+stp) $ V.snoc acc $ (V.!) arr (normalize i)
            else
              acc

      getSliceReverse st en stp arr = loop upper V.empty
        where
          (lower,upper) = (min (max en (-1)) (len-1), min (max st (-1)) (len-1))

          loop i acc =
            if lower < i
              then loop (i+stp) $ V.snoc acc $ (V.!) arr (normalize i)
            else
              acc


toPathKey :: String -> Text -> String
toPathKey loc key = loc ++ "['" ++ escapeEscapees (T.unpack key) ++ "']"
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
