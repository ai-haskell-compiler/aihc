module Data.Container.Refined.Conversion where

import           Data.Constraint (Dict(..))
import           Data.Container.Refined.Hashable
import           Data.Container.Refined.Proofs
import qualified Data.HashMap.Lazy as HashMap
import           Data.HashMap.Common.Refined
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import           Data.IntMap.Common.Refined
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import           Data.Map.Common.Refined
import           Data.Proxy
import           Data.Reflection
import qualified Data.Set as Set
import           Data.Type.Equality ((:~:)(..))
import           Unsafe.Coerce


unsafeWrapSet :: forall r r' a. KnownSet r' a => Proxy r' -> Set r a
unsafeWrapSet _ = case unsafeCoerce Refl :: r :~: r' of Refl -> Dict
{-# INLINE unsafeWrapSet #-}

unsafeWrapIntSet :: forall r r'. KnownIntSet r' => Proxy r' -> IntSet r
unsafeWrapIntSet _ = case unsafeCoerce Refl :: r :~: r' of Refl -> Dict
{-# INLINE unsafeWrapIntSet #-}

unsafeWrapHashSet :: forall r r' a. KnownHashSet r' a => Proxy r' -> HashSet r a
unsafeWrapHashSet _ = case unsafeCoerce Refl :: r :~: r' of Refl -> Dict
{-# INLINE unsafeWrapHashSet #-}

set2IntSet :: forall s. KnownSet s Int => IntSet s
set2IntSet = reify
  (IntSet.fromDistinctAscList $ Set.toAscList $ reflect $ Proxy @s)
  unsafeWrapIntSet

map2IntMap :: forall s a. Map s Int a -> IntMap s a
map2IntMap (Map m) = IntMap $ IntMap.fromDistinctAscList $ Map.toAscList m

set2HashSet :: forall s a. (Hashable a, KnownSet s a) => HashSet s a
set2HashSet = reify
  (HashSet.fromList $ Set.toList $ reflect $ Proxy @s)
  unsafeWrapHashSet

map2HashMap :: forall s k a. Hashable k => Map s k a -> HashMap s k a
map2HashMap (Map m) = HashMap $ HashMap.fromList $ Map.toList m

intSet2Set :: forall s. KnownIntSet s => Set s Int
intSet2Set = reify
  (Set.fromDistinctAscList $ IntSet.toAscList $ reflect $ Proxy @s)
  unsafeWrapSet

intMap2Map :: forall s a. IntMap s a -> Map s Int a
intMap2Map (IntMap m) = Map $ Map.fromDistinctAscList $ IntMap.toAscList m

intSet2HashSet :: forall s. KnownIntSet s => HashSet s Int
intSet2HashSet = reify
  (HashSet.fromList $ IntSet.toList $ reflect $ Proxy @s)
  unsafeWrapHashSet

intMap2HashMap :: forall s a. IntMap s a -> HashMap s Int a
intMap2HashMap (IntMap m) = HashMap $ HashMap.fromList $ IntMap.toList m

hashSet2Set :: forall s a. (Ord a, KnownHashSet s a) => Set s a
hashSet2Set = reify
  (Set.fromList $ HashSet.toList $ reflect $ Proxy @s)
  unsafeWrapSet

hashMap2Map :: forall s k a. Ord k => HashMap s k a -> Map s k a
hashMap2Map (HashMap m) = Map $ Map.fromList $ HashMap.toList m

hashSet2IntSet :: forall s. KnownHashSet s Int => IntSet s
hashSet2IntSet = reify
  (IntSet.fromList $ HashSet.toList $ reflect $ Proxy @s)
  unsafeWrapIntSet

hashMap2IntMap :: forall s a. HashMap s Int a -> IntMap s a
hashMap2IntMap (HashMap m) = IntMap $ IntMap.fromList $ HashMap.toList m
