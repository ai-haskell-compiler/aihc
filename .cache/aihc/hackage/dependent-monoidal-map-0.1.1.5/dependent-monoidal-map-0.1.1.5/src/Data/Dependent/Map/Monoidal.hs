{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Data.Dependent.Map.Monoidal where

import Data.Aeson
import Data.Coerce
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare
import Data.GADT.Show
import Data.Kind (Type)
import Data.Maybe
import Data.Semigroup
import Data.Some hiding (This)
import Data.Type.Equality
import Text.Read
import Prelude hiding (lookup, map)

newtype MonoidalDMap (f :: k -> Type) (g :: k -> Type) = MonoidalDMap { unMonoidalDMap :: DMap f g }

-- Temporary shim to avoid making changes to dependent-sum and dependent-map.
-- TODO: Finalise constraints-extras and optionally get it upstreamed into constraints.
-- Then actually get these instances into the real DSum and DMap,
-- killing off EqTag, OrdTag, ShowTag and ReadTag.
newtype FakeDSum f g = FakeDSum { unFakeDSum :: DSum f g }

instance (GEq f, Has' Eq f g) => Eq (FakeDSum f g) where
  FakeDSum ((k :: k a) :=> v) == FakeDSum (k' :=> v') = case geq k k' of
    Nothing -> False
    Just Refl -> has' @Eq @g k (v == v')

instance (GCompare f, Has' Eq f g, Has' Ord f g) => Ord (FakeDSum f g) where
  compare (FakeDSum (k :=> v)) (FakeDSum (k' :=> v')) = case gcompare k k' of
    GLT -> LT
    GGT -> GT
    GEQ -> has' @Ord @g k (compare v v')

-- NB: We're not going to show/parse the "FakeDSum" constructor, because this whole datatype is a temporary shim.
instance (ForallF Show f, Has' Show f g) => Show (FakeDSum f g) where
  showsPrec p (FakeDSum ((k :: f a) :=> v)) = showParen (p >= 10)
    ( whichever @Show @f @a (showsPrec 0 k)
    . showString " :=> "
    . has' @Show @g k (showsPrec 1 v)
    )

instance (GRead f, Has' Read f g) => Read (FakeDSum f g) where
  readsPrec p = readParen (p > 1) $ \s ->
    concat
      [ getGReadResult withTag $ \tag ->
          [ (FakeDSum (tag :=> val), rest'')
          | (val, rest'') <- has' @Read @g tag $ readsPrec 1 rest'
          ]
      | (withTag, rest) <- greadsPrec p s
      , (":=>", rest') <- lex rest
      ]

instance forall f g. (Has' Eq f g, GCompare f) => Eq (MonoidalDMap f g) where
  MonoidalDMap m == MonoidalDMap m' =
    (coerce (DMap.toList m) :: [FakeDSum f g]) == (coerce (DMap.toList m'))

instance forall f g. (Has' Eq f g, Has' Ord f g, GCompare f) => Ord (MonoidalDMap f g) where
  compare (MonoidalDMap m) (MonoidalDMap m') =
    compare (coerce (DMap.toList m) :: [FakeDSum f g]) (coerce (DMap.toList m'))

instance (Show (FakeDSum k f)) => Show (MonoidalDMap k f) where
    showsPrec p m = showParen (p>10)
        ( showString "fromList "
        . showsPrec 11 (coerce (toList m) :: [FakeDSum k f])
        )

instance (GCompare k, Read (FakeDSum k f)) => Read (MonoidalDMap k f) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return . MonoidalDMap . DMap.fromList $ coerce (xs :: [FakeDSum k f])
  readListPrec = readListPrecDefault

deriving instance (ToJSON (DMap f g)) => ToJSON (MonoidalDMap f g)

instance (Has' Semigroup f g, GCompare f) => Semigroup (MonoidalDMap f g) where
  (MonoidalDMap m) <> (MonoidalDMap n) =
    MonoidalDMap (DMap.unionWithKey (\f (u :: g a) v -> has' @Semigroup @g f (u <> v)) m n)

instance (Has' Semigroup f g, GCompare f) => Monoid (MonoidalDMap f g) where
  mempty = empty

deriving instance (FromJSON (DMap f g)) => FromJSON (MonoidalDMap f g)

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: MonoidalDMap k f
empty = MonoidalDMap DMap.empty

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1
singleton :: k v -> f v -> MonoidalDMap k f
singleton k x = MonoidalDMap (DMap.singleton k x)

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is the map empty?
null :: MonoidalDMap k f -> Bool
null (MonoidalDMap m) = DMap.null m

-- | /O(1)/. The number of elements in the map.
size :: MonoidalDMap k f -> Int
size (MonoidalDMap m) = DMap.size m

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
lookup :: forall k f v. GCompare k => k v -> MonoidalDMap k f -> Maybe (f v)
lookup k (MonoidalDMap m) = DMap.lookup k m


-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map

deleteFindMin :: MonoidalDMap k f -> (DSum k f, MonoidalDMap k f)
deleteFindMin (MonoidalDMap m) =
  case DMap.deleteFindMin m of
    (x, m') -> (x, MonoidalDMap m')

-- | /O(log n)/. Retrieves the minimal (key :=> value) entry of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
minViewWithKey :: forall k f . MonoidalDMap k f -> Maybe (DSum k f, MonoidalDMap k f)
minViewWithKey (MonoidalDMap m) =
  case DMap.minViewWithKey m of
    Nothing -> Nothing
    Just (x, m') -> Just (x, MonoidalDMap m')

-- | /O(log n)/. Retrieves the maximal (key :=> value) entry of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
maxViewWithKey :: forall k f . MonoidalDMap k f -> Maybe (DSum k f, MonoidalDMap k f)
maxViewWithKey (MonoidalDMap m) =
  case DMap.maxViewWithKey m of
    Nothing -> Nothing
    Just (x, m') -> Just (x, MonoidalDMap m')

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map

deleteFindMax :: MonoidalDMap k f -> (DSum k f, MonoidalDMap k f)
deleteFindMax (MonoidalDMap m) =
  case DMap.deleteFindMax m of
    (x, m') -> (x, MonoidalDMap m')

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: GCompare k => k a -> MonoidalDMap k f -> Bool
member k = isJust . lookup k

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
notMember :: GCompare k => k v -> MonoidalDMap k f -> Bool
notMember k m = not (member k m)

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
-- Consider using 'lookup' when elements may not be present.
find :: GCompare k => k v -> MonoidalDMap k f -> f v
find k m = case lookup k m of
    Nothing -> error "MonoidalDMap.find: element not in the map"
    Just v  -> v

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
findWithDefault :: GCompare k => f v -> k v -> MonoidalDMap k f -> f v
findWithDefault def k m = case lookup k m of
    Nothing -> def
    Just v  -> v

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: forall k f v. GCompare k => k v -> f v -> MonoidalDMap k f -> MonoidalDMap k f
insert k v (MonoidalDMap m) = MonoidalDMap (DMap.insert k v m)

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the entry @key :=> value@ into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the entry @key :=> f new_value old_value@.
insertWith :: GCompare k => (f v -> f v -> f v) -> k v -> f v -> MonoidalDMap k f -> MonoidalDMap k f
insertWith f k v (MonoidalDMap m) = MonoidalDMap (DMap.insertWith f k v m)

-- | Same as 'insertWith', but the combining function is applied strictly.
-- This is often the most desirable behavior.
insertWith' :: GCompare k => (f v -> f v -> f v) -> k v -> f v -> MonoidalDMap k f -> MonoidalDMap k f
insertWith' f k v (MonoidalDMap m) = MonoidalDMap (DMap.insertWith' f k v m)

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the entry @key :=> value@ into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the entry @key :=> f key new_value old_value@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
insertWithKey :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> MonoidalDMap k f -> MonoidalDMap k f
insertWithKey f k v (MonoidalDMap m) = MonoidalDMap (DMap.insertWithKey f k v m)

-- | Same as 'insertWithKey', but the combining function is applied strictly.
insertWithKey' :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> MonoidalDMap k f -> MonoidalDMap k f
insertWithKey' f k v (MonoidalDMap m) = MonoidalDMap (DMap.insertWithKey' f k v m)


-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
insertLookupWithKey :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> MonoidalDMap k f
                    -> (Maybe (f v), MonoidalDMap k f)
insertLookupWithKey f k v (MonoidalDMap m) =
  case DMap.insertLookupWithKey f k v m of
    (x, y) -> (x, MonoidalDMap y)

-- | /O(log n)/. A strict version of 'insertLookupWithKey'.
insertLookupWithKey' :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> MonoidalDMap k f
                     -> (Maybe (f v), MonoidalDMap k f)
insertLookupWithKey' f k v (MonoidalDMap m) =
  case DMap.insertLookupWithKey' f k v m of
    (x, y) -> (x, MonoidalDMap y)

{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: forall k f v. GCompare k => k v -> MonoidalDMap k f -> MonoidalDMap k f
delete k (MonoidalDMap m) = MonoidalDMap (DMap.delete k m)

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
adjust :: GCompare k => (f v -> f v) -> k v -> MonoidalDMap k f -> MonoidalDMap k f
adjust f k (MonoidalDMap m) = MonoidalDMap (DMap.adjust f k m)

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjustWithKey :: GCompare k => (k v -> f v -> f v) -> k v -> MonoidalDMap k f -> MonoidalDMap k f
adjustWithKey f k (MonoidalDMap m) = MonoidalDMap (DMap.adjustWithKey f k m)

-- | /O(log n)/. A strict version of 'adjustWithKey'.
adjustWithKey' :: GCompare k => (k v -> f v -> f v) -> k v -> MonoidalDMap k f -> MonoidalDMap k f
adjustWithKey' f k (MonoidalDMap m) = MonoidalDMap (DMap.adjustWithKey' f k m)

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: GCompare k => (f v -> Maybe (f v)) -> k v -> MonoidalDMap k f -> MonoidalDMap k f
update f k (MonoidalDMap m) = MonoidalDMap (DMap.update f k m)

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
updateWithKey :: forall k f v. GCompare k => (k v -> f v -> Maybe (f v)) -> k v -> MonoidalDMap k f -> MonoidalDMap k f
updateWithKey f k (MonoidalDMap m) = MonoidalDMap (DMap.updateWithKey f k m)

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
updateLookupWithKey :: forall k f v. GCompare k => (k v -> f v -> Maybe (f v)) -> k v -> MonoidalDMap k f -> (Maybe (f v), MonoidalDMap k f)
updateLookupWithKey f k (MonoidalDMap m) =
  case DMap.updateLookupWithKey f k m of
    (x, y) -> (x, MonoidalDMap y)

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: forall k f v. GCompare k => (Maybe (f v) -> Maybe (f v)) -> k v -> MonoidalDMap k f -> MonoidalDMap k f
alter f k (MonoidalDMap m) = MonoidalDMap (DMap.alter f k m)

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}

-- | /O(log n)/. Return the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map. Calls 'error' when
-- the key is not a 'member' of the map.
findIndex :: GCompare k => k v -> MonoidalDMap k f -> Int
findIndex k (MonoidalDMap m)
  = case DMap.lookupIndex k m of
      Nothing  -> error "MonoidalDMap.findIndex: element is not in the map"
      Just idx -> idx

-- | /O(log n)/. Lookup the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map.
lookupIndex :: forall k f v. GCompare k => k v -> MonoidalDMap k f -> Maybe Int
lookupIndex k (MonoidalDMap m) = DMap.lookupIndex k m

-- | /O(log n)/. Retrieve an element by /index/. Calls 'error' when an
-- invalid index is used.
elemAt :: Int -> MonoidalDMap k f -> DSum k f
elemAt i (MonoidalDMap m) = DMap.elemAt i m

-- | /O(log n)/. Update the element at /index/. Does nothing when an
-- invalid index is used.
updateAt :: (forall v. k v -> f v -> Maybe (f v)) -> Int -> MonoidalDMap k f -> MonoidalDMap k f
updateAt f i (MonoidalDMap m) = MonoidalDMap (DMap.updateAt f i m)

-- | /O(log n)/. Delete the element at /index/.
-- Defined as (@'deleteAt' i map = 'updateAt' (\k x -> 'Nothing') i map@).
deleteAt :: Int -> MonoidalDMap k f -> MonoidalDMap k f
deleteAt i (MonoidalDMap m) = MonoidalDMap (DMap.deleteAt i m)

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(log n)/. The minimal key of the map. Calls 'error' is the map is empty.
findMin :: MonoidalDMap k f -> DSum k f
findMin (MonoidalDMap m) = case DMap.lookupMin m of
  Just x -> x
  Nothing -> error "MonoidalDMap.findMin: empty map has no minimal element"

lookupMin :: MonoidalDMap k f -> Maybe (DSum k f)
lookupMin (MonoidalDMap m) = DMap.lookupMin m

-- | /O(log n)/. The maximal key of the map. Calls 'error' is the map is empty.
findMax :: MonoidalDMap k f -> DSum k f
findMax m = case lookupMax m of
  Just x -> x
  Nothing -> error "Map.findMax: empty map has no maximal element"

lookupMax :: MonoidalDMap k f -> Maybe (DSum k f)
lookupMax (MonoidalDMap m) = DMap.lookupMax m

-- | /O(log n)/. Delete the minimal key. Returns an empty map if the map is empty.
deleteMin :: MonoidalDMap k f -> MonoidalDMap k f
deleteMin (MonoidalDMap m) = MonoidalDMap (DMap.deleteMin m)

-- | /O(log n)/. Delete the maximal key. Returns an empty map if the map is empty.
deleteMax :: MonoidalDMap k f -> MonoidalDMap k f
deleteMax (MonoidalDMap m) = MonoidalDMap (DMap.deleteMax m)

-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: (forall v. k v -> f v -> Maybe (f v)) -> MonoidalDMap k f -> MonoidalDMap k f
updateMinWithKey f (MonoidalDMap m) = MonoidalDMap (DMap.updateMinWithKey f m)

-- | /O(log n)/. Update the value at the maximal key.
updateMaxWithKey :: (forall v. k v -> f v -> Maybe (f v)) -> MonoidalDMap k f -> MonoidalDMap k f
updateMaxWithKey f (MonoidalDMap m) = MonoidalDMap (DMap.updateMaxWithKey f m)

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWithKey' f == 'Prelude.foldl' ('unionWithKey' f) 'empty'@).
unionsWithKey :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> [MonoidalDMap k f] -> MonoidalDMap k f
unionsWithKey f ms = MonoidalDMap (DMap.unionsWithKey f (coerce ms))

{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}

-- | /O(n+m)/.
-- Union with a combining function.
unionWithKey :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> MonoidalDMap k f -> MonoidalDMap k f -> MonoidalDMap k f
unionWithKey f (MonoidalDMap m) (MonoidalDMap n) = MonoidalDMap (DMap.unionWithKey f m n)

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- | /O(m * log (n\/m + 1)), m <= n/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
difference :: GCompare k => MonoidalDMap k f -> MonoidalDMap k g -> MonoidalDMap k f
difference (MonoidalDMap m) (MonoidalDMap n) = MonoidalDMap (DMap.difference m n)

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWithKey :: GCompare k => (forall v. k v -> f v -> g v -> Maybe (f v)) -> MonoidalDMap k f -> MonoidalDMap k g -> MonoidalDMap k f
differenceWithKey f (MonoidalDMap m) (MonoidalDMap n) = MonoidalDMap (DMap.differenceWithKey f m n)

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}

-- | /O(m * log (n\/m + 1), m <= n/. Intersection with a combining function.
intersectionWithKey :: GCompare k => (forall v. k v -> f v -> g v -> h v) -> MonoidalDMap k f -> MonoidalDMap k g -> MonoidalDMap k h
intersectionWithKey f (MonoidalDMap m) (MonoidalDMap n) = MonoidalDMap (DMap.intersectionWithKey f m n)

{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' 'eqTagged')@).
--
isSubmapOf :: (GCompare k, Has' Eq k f) => MonoidalDMap k f -> MonoidalDMap k f -> Bool
isSubmapOf (MonoidalDMap m) (MonoidalDMap n) = DMap.isSubmapOf m n

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective keys and values.
-}
isSubmapOfBy :: GCompare k => (forall v. k v -> k v -> f v -> g v -> Bool) -> MonoidalDMap k f -> MonoidalDMap k g -> Bool
isSubmapOfBy f (MonoidalDMap m) (MonoidalDMap n) = DMap.isSubmapOfBy f m n

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' 'eqTagged'@).
isProperSubmapOf :: (GCompare k, Has' Eq k f) => MonoidalDMap k f -> MonoidalDMap k f -> Bool
isProperSubmapOf (MonoidalDMap m) (MonoidalDMap n) = DMap.isProperSubmapOf m n

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective keys and values.
-}
isProperSubmapOfBy :: GCompare k => (forall v. k v -> k v -> f v -> g v -> Bool) -> MonoidalDMap k f -> MonoidalDMap k g -> Bool
isProperSubmapOfBy f (MonoidalDMap m) (MonoidalDMap n) = DMap.isProperSubmapOfBy f m n

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
filterWithKey :: GCompare k => (forall v. k v -> f v -> Bool) -> MonoidalDMap k f -> MonoidalDMap k f
filterWithKey p (MonoidalDMap m) = MonoidalDMap (DMap.filterWithKey p m)

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partitionWithKey :: GCompare k => (forall v. k v -> f v -> Bool) -> MonoidalDMap k f -> (MonoidalDMap k f, MonoidalDMap k f)
partitionWithKey p (MonoidalDMap m) =
  case DMap.partitionWithKey p m of
    (x, y) -> (MonoidalDMap x, MonoidalDMap y)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
mapMaybeWithKey :: GCompare k => (forall v. k v -> f v -> Maybe (g v)) -> MonoidalDMap k f -> MonoidalDMap k g
mapMaybeWithKey f (MonoidalDMap m) = MonoidalDMap (DMap.mapMaybeWithKey f m)

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: GCompare k =>
  (forall v. k v -> f v -> Either (g v) (h v)) -> MonoidalDMap k f -> (MonoidalDMap k g, MonoidalDMap k h)
mapEitherWithKey f (MonoidalDMap m) =
  case DMap.mapEitherWithKey f m of
    (x, y) -> (MonoidalDMap x, MonoidalDMap y)

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}

-- | /O(n)/. Map a function over all values in the map.
map :: (forall v. f v -> g v) -> MonoidalDMap k f -> MonoidalDMap k g
map f (MonoidalDMap m) = MonoidalDMap (DMap.map f m)

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (forall v. k v -> f v -> g v) -> MonoidalDMap k f -> MonoidalDMap k g
mapWithKey f (MonoidalDMap m) = MonoidalDMap (DMap.mapWithKey f m)

-- | /O(n)/.
-- @'traverseWithKey' f m == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
traverseWithKey :: Applicative t => (forall v. k v -> f v -> t (g v)) -> MonoidalDMap k f -> t (MonoidalDMap k g)
traverseWithKey f (MonoidalDMap m) = fmap MonoidalDMap (DMap.traverseWithKey f m)

-- | /O(n)/. The function 'mapAccumLWithKey' threads an accumulating
-- argument throught the map in ascending order of keys.
mapAccumLWithKey :: (forall v. a -> k v -> f v -> (a, g v)) -> a -> MonoidalDMap k f -> (a, MonoidalDMap k g)
mapAccumLWithKey f x (MonoidalDMap m) =
  case DMap.mapAccumLWithKey f x m of
    (y, m') -> (y, MonoidalDMap m')

-- | /O(n)/. The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (forall v. a -> k v -> f v -> (a, g v)) -> a -> MonoidalDMap k f -> (a, MonoidalDMap k g)
mapAccumRWithKey f x (MonoidalDMap m) =
  case DMap.mapAccumRWithKey f x m of
    (y, m') -> (y, MonoidalDMap m')

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
mapKeysWith :: GCompare k2 => (forall v. k2 v -> f v -> f v -> f v) -> (forall v. k1 v -> k2 v) -> MonoidalDMap k1 f -> MonoidalDMap k2 f
mapKeysWith c f (MonoidalDMap m) = MonoidalDMap (DMap.mapKeysWith c f m)

-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
mapKeysMonotonic :: (forall v. k1 v -> k2 v) -> MonoidalDMap k1 f -> MonoidalDMap k2 f
mapKeysMonotonic f (MonoidalDMap m) = MonoidalDMap (DMap.mapKeysMonotonic f m)

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Post-order fold.  The function will be applied from the lowest
-- value to the highest.
foldrWithKey :: (forall v. k v -> f v -> b -> b) -> b -> MonoidalDMap k f -> b
foldrWithKey f x (MonoidalDMap m) = DMap.foldrWithKey f x m

-- | /O(n)/. Pre-order fold.  The function will be applied from the highest
-- value to the lowest.
foldlWithKey :: (forall v. b -> k v -> f v -> b) -> b -> MonoidalDMap k f -> b
foldlWithKey f x (MonoidalDMap m) = DMap.foldlWithKey f x m

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: MonoidalDMap k f -> [Some k]
keys (MonoidalDMap m) = DMap.keys m

-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
assocs :: MonoidalDMap k f -> [DSum k f]
assocs (MonoidalDMap m) = DMap.assocs m

{--------------------------------------------------------------------
  Lists
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
fromListWithKey :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> [DSum k f] -> MonoidalDMap k f
fromListWithKey f xs = MonoidalDMap (DMap.fromListWithKey f xs)

-- | /O(n)/. Convert to a list of key\/value pairs.
toList :: MonoidalDMap k f -> [DSum k f]
toList (MonoidalDMap m) = DMap.toList m

-- | /O(n)/. Convert to an ascending list.
toAscList :: MonoidalDMap k f -> [DSum k f]
toAscList (MonoidalDMap m) = DMap.toAscList m

-- | /O(n)/. Convert to a descending list.
toDescList :: MonoidalDMap k f -> [DSum k f]
toDescList (MonoidalDMap m) = DMap.toDescList m

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: GEq k => (forall v. k v -> f v -> f v -> f v) -> [DSum k f] -> MonoidalDMap k f
fromAscListWithKey f xs = MonoidalDMap (DMap.fromAscListWithKey f xs)

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}

-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
split :: forall k f v. GCompare k => k v -> MonoidalDMap k f -> (MonoidalDMap k f, MonoidalDMap k f)
split k (MonoidalDMap m) =
  case DMap.split k m of
    (x, y) -> (MonoidalDMap x, MonoidalDMap y)
{-# INLINABLE split #-}

-- | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
splitLookup :: forall k f v. GCompare k => k v -> MonoidalDMap k f -> (MonoidalDMap k f, Maybe (f v), MonoidalDMap k f)
splitLookup k (MonoidalDMap m) =
  case DMap.splitLookup k m of
    (x, v, y) -> (MonoidalDMap x, v, MonoidalDMap y)

-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format. See 'showTreeWith'.
showTree :: (GShow k, Has' Show k f) => MonoidalDMap k f -> String
showTree (MonoidalDMap m) = DMap.showTree m

{- | /O(n)/. The expression (@'showTreeWith' showelem hang wide map@) shows
 the tree that implements the map. Elements are shown using the @showElem@ function. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: (forall v. k v -> f v -> String) -> Bool -> Bool -> MonoidalDMap k f -> String
showTreeWith showElem hang wide (MonoidalDMap m) = DMap.showTreeWith showElem hang wide m

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}

-- | /O(n)/. Test if the internal map structure is valid.
valid :: GCompare k => MonoidalDMap k f -> Bool
valid (MonoidalDMap m) = DMap.valid m
