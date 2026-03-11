module Data.Mapping.Util where

import Data.Functor.Compose (Compose(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


-- | inserts key with value only if absent, returns map if changed
insertIfAbsent :: Ord k => k -> v -> Map k v -> (v, Maybe (Map k v))
insertIfAbsent k v = let
  f (Just x) = (x, Nothing)
  f Nothing  = (v, Just (Just v))
  in getCompose . M.alterF (Compose . f) k


-- | For use in maps where we don't want to store default values
nonDefault :: Eq a => a -> a -> Maybe a
nonDefault d x
  | d == x    = Nothing
  | otherwise = Just x


-- | Helper function (not exported)
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating f x y = f x == f y
