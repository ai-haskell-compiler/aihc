module Data.Graph.Comfort.Map where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Tuple.HT (swap)
import Data.Maybe (mapMaybe)

import qualified Prelude as P
import Prelude hiding (curry, uncurry, flip)


-- | New improved ugly version with caller function name
type Caller = String


checkedLookup ::
  (Ord k, Show k) => Caller -> Map k v -> k -> v
checkedLookup c m k =
  case Map.lookup k m of
    Nothing ->
      error $ "checkedLookup error in " ++ c ++ "\n"
              ++ "key: " ++ show k  ++ "\n"
              ++ "keys in map:\n" ++ unlines (map show (Map.keys m)) ++ "\n"
    Just x -> x

{- |
The set of keys must be equal and this is checked dynamically.
-}
checkedZipWith ::
  (Ord k) =>
  Caller ->
  (a -> b -> c) ->
  Map k a -> Map k b -> Map k c
checkedZipWith caller f ma mb =
  if Map.keysSet ma == Map.keysSet mb
    then Map.intersectionWith f ma mb
    else error $
            "checkedZipWith called by function " ++ caller ++
            ": key sets differ"


reverse :: (Ord b) => Map a b -> Map b a
reverse = Map.fromList . map swap . Map.toList

-- Map.fromSet is available from containers-0.5
fromSet ::
   (Ord key) => (key -> a) -> Set key -> Map key a
fromSet f = Map.fromAscList . map (\k -> (k, f k)) . Set.toAscList

differenceSet ::
   (Ord key) => Map key a -> Set key -> Map key a
differenceSet m s = Map.difference m (fromSet (const ()) s)

intersectionSet ::
   (Ord key) => Map key a -> Set key -> Map key a
intersectionSet m s = Map.intersection m (fromSet (const ()) s)



curry ::
   (Ord k0, Ord k1) =>
   Caller ->
   (k -> (k0, k1)) ->
   Map k a -> Map k0 (Map k1 a)
curry caller f =
   Map.unionsWith (Map.unionWith (error $ caller ++ ".curry: duplicate key")) .
   Map.elems .
   Map.mapWithKey
      (\k a ->
         case f k of
            (k0, k1) -> Map.singleton k0 $ Map.singleton k1 a)

uncurry ::
   (Ord k) =>
   Caller ->
   (k0 -> k1 -> k) ->
   Map k0 (Map k1 v) -> Map k v
uncurry caller f =
   Map.unionsWith (error $ caller ++ ".uncurry: duplicate key") .
   Map.elems .
   Map.mapWithKey (Map.mapKeys . f)

flip ::
   (Ord k0, Ord k1) =>
   Map k0 (Map k1 a) -> Map k1 (Map k0 a)
flip =
   Map.unionsWith (Map.unionWith (error $ "Map.flip: duplicate key")) .
   Map.elems . Map.mapWithKey (fmap . Map.singleton)


mapMaybeKeys ::
   (Ord k1) =>
   (k0 -> Maybe k1) ->
   Map k0 a -> Map k1 a
mapMaybeKeys f =
   Map.fromList . mapMaybe (\(k,a) -> fmap (P.flip (,) a) $ f k) . Map.toList


compose :: (Ord a, Ord b) => Map b c -> Map a b -> Map a c
compose bc ab = Map.mapMaybe (P.flip Map.lookup bc) ab
