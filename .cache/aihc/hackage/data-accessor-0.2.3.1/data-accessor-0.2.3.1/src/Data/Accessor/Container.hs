{- |
This module allows to access elements of arrays, sets and finite maps
like elements of records.
This is especially useful for working with nested structures
consisting of arrays, sets, maps and records.

Maybe we should move it to a separate package,
then we would not need to import @array@ and @containers@ package.
-}
module Data.Accessor.Container
   (array, set,
    mapDefault, mapMaybe,
    intMapDefault, intMapMaybe,
   ) where

import qualified Data.Accessor.Basic as Accessor

import Data.Ix (Ix, )
import qualified Data.Array  as Array
import qualified Data.Set    as Set
import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap

import Prelude hiding (map)


array :: Ix i => i -> Accessor.T (Array.Array i e) e
array i = Accessor.fromSetGet (\e a -> a Array.// [(i,e)]) (Array.! i)

{- |
Treat a Set like a boolean array.
-}
set :: Ord a => a -> Accessor.T (Set.Set a) Bool
set a =
   Accessor.fromSetGet
      (\b -> if b then Set.insert a else Set.delete a)
      (Set.member a)

{- |
Treats a finite map like an infinite map,
where all undefined elements are replaced by a default value.
-}
mapDefault :: Ord key => elem -> key -> Accessor.T (Map.Map key elem) elem
mapDefault deflt key =
   Accessor.fromSetGet (Map.insert key) (Map.findWithDefault deflt key)

{- |
Treats a finite map like an infinite map,
where all undefined elements are 'Nothing'
and defined elements are 'Just'.
-}
mapMaybe :: Ord key => key -> Accessor.T (Map.Map key elem) (Maybe elem)
mapMaybe key =
   Accessor.fromSetGet
      (\e m -> maybe (Map.delete key m) (flip (Map.insert key) m) e)
      (Map.lookup key)

intMapDefault :: elem -> Int -> Accessor.T (IntMap.IntMap elem) elem
intMapDefault deflt key =
   Accessor.fromSetGet (IntMap.insert key) (IntMap.findWithDefault deflt key)

intMapMaybe :: Int -> Accessor.T (IntMap.IntMap elem) (Maybe elem)
intMapMaybe key =
   Accessor.fromSetGet
      (\e m -> maybe (IntMap.delete key m) (flip (IntMap.insert key) m) e)
      (IntMap.lookup key)
