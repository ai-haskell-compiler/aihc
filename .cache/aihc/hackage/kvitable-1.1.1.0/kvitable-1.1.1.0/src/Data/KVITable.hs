{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'KVITable' is similar to a 'Data.Map', but the keys for a
-- 'KVITable' are made up of sequences of @Key=Val@ values.  The
-- primary use of a 'KVITable' is for rendering the information in
-- various configurations and formats, although it may be used like
-- any other container.

module Data.KVITable
  (
    KVITable(KVITable)
  , Key
  , KeyVal
  , KeyVals
  , KeySpec
  , fromList
  , toList
  , Data.KVITable.lookup
  , lookup'
  , keyVals
  , keyValGen
  , valueColName
  , insert
  , insertWith
  , foldlInsert
  , foldlInsertWith
  , Data.KVITable.filter
  , adjust
  , adjustWithKey
  , delete
  , update
  , updateWithKey
  , rows
  )
where

import           Data.Function ( on )
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Name
import qualified GHC.Exts
import           Lens.Micro ( Lens' )

import           Data.KVITable.Internal.Helpers


-- | The core KeyValue Indexed Table.  This table is similar to a Map,
-- but the values are indexed by a list of Key+Value combinations, and
-- the table contents can be sparse.

-- KWQ: make fields strict?  check with tasty-bench

data KVITable v = KVITable
  { keyvals      :: KeyVals -- ^ allowed value for keys (in order)

  , keyvalGen    :: Key -> KeyVal
    -- ^ Function to generate the keyval if the keyval is not
    -- explicitly provided.  Provided with the Key and returns the
    -- KeyVal that should be used.

  , contents     :: Map.Map KeySpec v
    -- ^ Internal contents of the KVITable

    -- The invariant for the KVITable contents is that each KeySpec
    -- contains all keys listed in keyvals (in the same order) with
    -- the defaultKeyVal for any keys not explicitly provided for that
    -- value.

  , valuecolName :: Named HTMLStyle "column header" -- ^ name of the value cells
  }

instance Eq v => Eq (KVITable v) where
  -- n.b. keyvals (i.e. metadata) are _not_ used for equality, only contents
  (==) = (==) `on` contents

instance Show v => Show (KVITable v) where
  show t = "KVITable {" <>
           " keyvals = " <> show (keyvals t) <>
           " contents = " <> show (contents t) <>
           ", valuecolName = " <> show (valuecolName t) <>
           "}"

-- | The 'Key' is the first half of a tuple that makes up the list of
-- keys (the 'KeySpec').  The second half is the 'KeyVal'.
type Key = Name "Key"

-- | The 'KeyVal' is the first half of a tuple that makes up the list of
-- keys (the 'KeySpec').  The first half is the 'Key'.
type KeyVal = Name "KeyVal"

-- | The 'KeySpec' is the list of tuples and defines the unique key
-- for a value in the 'KVITable'.
type KeySpec = [ (Key,  KeyVal ) ]

-- | The 'KeyVals' specifies all valid values for a particular 'Key'
-- in the 'KVITable'.  The set of 'KeyVals' can be provided at the
-- initialization of the 'KVITable' to ensure specific values are
-- considered (especially if rendering includes blank rows or
-- columns); if entries are added to the table with a 'KeyVal'
-- previously unknown for the 'Key', the 'KeyVals' for the table is
-- automatically updated to include the new 'KeyVal'.
type KeyVals = [ (Key, [KeyVal]) ]

-- | The KVITable semigroup is left biased (same as Data.Map).  Note
-- that joining tables can result in a table that has a different
-- keyVals sequence than either input table.

instance Semigroup (KVITable v) where
  a <> b = foldl foldlInsert
           (mempty { valuecolName = valuecolName a
                   , keyvals = keyvals a
                   })
           (toList b <> toList a)

instance Monoid (KVITable v) where
  mempty = KVITable { keyvals = mempty
                    , keyvalGen = const ""
                    , contents = mempty
                    , valuecolName = "Value"
                    }

instance Functor KVITable where
  fmap f t = KVITable { contents = fmap f (contents t)
                      , keyvalGen = keyvalGen t
                      , keyvals = keyvals t
                      , valuecolName = valuecolName t
                      }

instance Foldable KVITable where
  foldMap f = foldMap f . contents

instance Traversable KVITable where
  traverse f t = (\c -> KVITable { contents = c
                                 , valuecolName = valuecolName t
                                 , keyvals = keyvals t
                                 , keyvalGen = keyvalGen t
                                 }
                 ) <$> traverse f (contents t)

instance GHC.Exts.IsList (KVITable v) where
  type Item (KVITable v) = (KeySpec, v)
  fromList = foldl foldlInsert mempty
  toList = GHC.Exts.toList . contents


-- | Converts a list of @([(Key,Val)], Value)@ tuples to a KVI table.

fromList :: [ GHC.Exts.Item (KVITable v) ] -> KVITable v
fromList = GHC.Exts.fromList

-- | Converts a KVI table to a list of @([(Key,Val)], Value)@ tuples.

toList :: KVITable v -> [ GHC.Exts.Item (KVITable v) ]
toList = GHC.Exts.toList


-- | Fetch or set the keyvals list via lenses. Note that setting the keyval list
-- will drop any current contents in the table that do not have entries in the
-- keyvals list; existing entries in the table under the old keys are NOT
-- migrated to the new table entries.

keyVals :: Lens' (KVITable v) KeyVals
keyVals f t = (\kvs ->
                 t { keyvals = kvs
                   , contents =
                     let inKVS spec _val =
                           length kvs == length spec
                           && and (keymatch <$> zip spec kvs)
                         keymatch ((sk,sv), (k,vs)) = sk == k && sv `elem` vs
                     in Map.filterWithKey inKVS (contents t)
                   }
              ) <$> f (keyvals t)


-- | Fetch or set the default 'KeyVal' generator for this 'KVITable'

keyValGen :: Lens' (KVITable v) (Key -> KeyVal)
keyValGen f t = (\n -> t { keyvalGen = n } ) <$> f (keyvalGen t)

-- | Fetch or set the column name for the actual value cell in the
-- 'KVITable'.

valueColName :: Lens' (KVITable v) (Named HTMLStyle "column header")
valueColName f t = (\n -> t { valuecolName = n } ) <$> f (valuecolName t)


-- | Retrieve an entry from the KVITable given a keyspec.  The keyspec
-- may be minimally specified (i.e. it does not need to contain keys
-- whose value is the default key value) and it may present the keys
-- out of order and the lookup will still succeed (if there is a value
-- for the normalized keyspec), but it will be faster to use the
-- normalized key directly.
--
-- The performance of lookup degrades exponentially on a sparse KVITable: an
-- unset KVITable entry is indistinguishable from a non-normalized keyspec, so if
-- the initial query fails, the keyspec is normalized and then the lookup is
-- re-attempted.  This may be fine for an individual query, but for large numbers
-- of queries this is excessively slow.  To mitigate this, the lookup' can be
-- used, which assumes the use of a normalized key.

lookup :: KeySpec -> KVITable v -> Maybe v
lookup keyspec t = case Map.lookup keyspec $ contents t of
                     Just v -> Just v
                     Nothing ->
                       -- keyspec might be under-specified or in a different order
                       let ks = normalizeKeySpec t keyspec
                       in Map.lookup ks $ contents t

normalizeKeySpec :: KVITable v -> KeySpec -> KeySpec
normalizeKeySpec t keyspec =
  let keyandval s (k,vs) = case L.lookup k keyspec of
        Just v ->
          if v `elem` vs then snoc s (k,v)
          else s -- no level added, so this should never match in the Map
        Nothing -> snoc s (k, keyvalGen t k)
  in foldl keyandval mempty (keyvals t)


-- | Like 'lookup', but assumes a normalized key (all key elements specified, and
-- in the proper order).  Faster than 'lookup', but will return false negatives
-- if not used with a normalized key.

lookup' :: KeySpec -> KVITable v -> Maybe v
lookup' keyspec = Map.lookup keyspec . contents

-- | Inserts a new cell value into the table at the specified keyspec
-- location.  The keyspec may be minimally specified and out-of-order.
--
-- This may be an expensive operation if it has to extend the keyvals
-- for the table.  In general, insertion is expected to be less
-- frequent than lookups so computation costs are biased towards the
-- insertion operation.
--
-- This is equivalent to @insertWith const@.

insert :: KeySpec -> v -> KVITable v -> KVITable v
insert = insertWith const

-- | Inserts a new cell value into the table at the specified keyspec
-- location, if no value exists at that location. Otherwise, the old and
-- new value are combined via @f new old@.  The keyspec may be minimally
-- specified and out-of-order.
--
-- This may be an expensive operation if it has to extend the keyvals
-- for the table.  In general, insertion is expected to be less
-- frequent than lookups so computation costs are biased towards the
-- insertion operation.

insertWith :: (v -> v -> v) -> KeySpec -> v -> KVITable v -> KVITable v
insertWith f keyspec val t = endsetWith f t val (keyvals t) keyspec mempty mempty

remainingKeyValDefaults :: KVITable v -> [(Key,a)] -> KeySpec
remainingKeyValDefaults t = fmap (\(k,_) -> (k, keyvalGen t k))

addDefVal :: KVITable v -> (Key, [KeyVal]) ->  (Key, [KeyVal])
addDefVal t e@(k,vs) = if (keyvalGen t k) `elem` vs
                       then e
                       else (k, keyvalGen t k : vs)

endsetWith :: (v -> v -> v) -> KVITable v -> v -> KeyVals -> KeySpec -> KeySpec -> KeyVals -> KVITable v
endsetWith f t val rkv [] tspec kvbld =
        -- Reached the end of the user's keyspec but there are more
        -- known keyvals in this KVITable, so add the entry with the
        -- default KeyVal for the remaining keyspec (and ensure the
        -- default KeyVal is listed in the table's keyvals).
        let spec = tspec <> remainingKeyValDefaults t rkv
        in t { contents = Map.insertWith f spec val (contents t)
             , keyvals = kvbld <> (addDefVal t <$> rkv)
             }

endsetWith f t val [] spec tspec kvbld =
  -- Reached the end of the known keyvals for this table but the
  -- user's keyspec has additional elements.  This should extend
  -- the tables keyvals with the remaining keyspec; also all
  -- existing table values should be pushed out to use the
  -- default values for the new keys in their keyspec.
  let spec' = tspec <> spec
      keySpecElemToKeyVals (k,v) = (k, if null curTblList
                                       then single v
                                       else [v, keyvalGen t k])
      keyvals' = kvbld <> (keySpecElemToKeyVals <$> spec)
      curTblList = Map.toList $ contents t
      defaultsExtension = remainingKeyValDefaults t spec
      updTblList = fmap (\(ks,v) -> (ks <> defaultsExtension, v)) curTblList
  in t { contents = Map.insertWith f spec' val $ Map.fromList updTblList
       , keyvals = keyvals'
       }

endsetWith f t val kvs@((k,vs):rkvs) ((sk,sv):srs) tspec kvbld =
  if k == sk
  then let kv' = if sv `elem` vs
                 then snoc kvbld (k, vs)
                 else snoc kvbld (k, sv : vs)
       in endsetWith f t val rkvs srs (snoc tspec (k,sv)) kv'
  else
    -- re-arrange user spec crudely by throwing invalid
    -- candidates to the end and retrying.  This isn't
    -- necessarily efficient, but keyspecs aren't expected to be
    -- longer than about a dozen entries.
    if sk `elem` (fst <$> rkvs) && k `elem` (fst <$> srs)
    then endsetWith f t val kvs (snoc srs (sk,sv)) tspec kvbld
    else
      if any (`elem` (fst <$> kvs)) (fst <$> srs)
      then endsetWith f t val kvs (snoc srs (sk,sv)) tspec kvbld
      else
        let defVal = keyvalGen t k
            vs' = if defVal `elem` vs then vs else (defVal : vs)
        in endsetWith f t val rkvs ((sk,sv):srs) (snoc tspec (k,defVal)) (snoc kvbld (k,vs'))


-- | foldlInsert is a convenience function that can be specified
-- as the function argument of a foldl operation over the list form of
-- a KVITable to generate the associated KVITable.
--
-- This is equivalent to @foldlInsertWith const@.

foldlInsert :: KVITable v -> (KeySpec, v) -> KVITable v
foldlInsert = foldlInsertWith const

-- | foldlInsertWith is a convenience function that, when curried with a
-- "combining" function, can be specified as the function argument of a foldl
-- operation over the list form of a KVITable to generate the associated
-- KVITable.

foldlInsertWith :: (v -> v -> v) -> KVITable v -> (KeySpec, v) -> KVITable v
foldlInsertWith f t (k,v) = insertWith f k v t


-- | Filter 'KVITable' to retain only the elements that satisfy some predicate.

filter :: ((KeySpec, v) -> Bool) -> KVITable v -> KVITable v
filter f t = foldl chkInsert (emptyClone t) $ toList t
  where emptyClone o = o { contents = mempty }
        chkInsert o (k,v) = if f (k,v) then insert k v o else o

-- | Delete the value at the specified keyspec location in the
-- 'KVITable'.  If the keyspec does not exist, the original table is
-- returned.

delete :: KeySpec -> KVITable v -> KVITable v
delete k t = t { contents = Map.delete (normalizeKeySpec t k) $ contents t }

-- | Adjust a value at the specified keyspec; return the original
-- 'KVITable' if that keyspec is not found in the table.

adjustWithKey :: (KeySpec -> v -> v) -> KeySpec -> KVITable v -> KVITable v
adjustWithKey f k t =
  t { contents = Map.adjustWithKey f (normalizeKeySpec t k) $ contents t }

-- | Adjust a value at the specified keyspec; return the original
-- 'KVITable' if that keyspec is not found in the table.

adjust :: (v -> v) -> KeySpec -> KVITable v -> KVITable v
adjust f k t = t { contents = Map.adjust f (normalizeKeySpec t k) $ contents t }

-- | Update the 'KVITable' to remove or set a new value for the
-- specified entry if the updating function returns @Nothing@ or @Just
-- v@, respectively.  The update function is passed both the keyspec
-- and the current value at that key.  If the value does not exist in
-- the table, the original table is returned.

updateWithKey :: (KeySpec -> v -> Maybe v) -> KeySpec -> KVITable v -> KVITable v
updateWithKey f k t =
  t { contents = Map.updateWithKey f (normalizeKeySpec t k) $ contents t }

-- | Update the 'KVITable' to remove or set a new value for the
-- specified entry if the updating function returns @Nothing@ or @Just
-- v@, respectively.  The update function is passed the value for the
-- keyspec to be updated. If the value does not exist in the table,
-- the original table is returned.

update :: (v -> Maybe v) -> KeySpec -> KVITable v -> KVITable v
update f k t = t { contents = Map.update f (normalizeKeySpec t k) $ contents t }

-- | The 'rows' function returns a set of rows for the 'KVITable' as a
-- list structure, where each returned entry is a different row.  A row
-- consists of the /values/ of the keys for that row followed by the
-- value of the entry (to get the names of the keys, use 'keyVals').

rows :: KVITable v -> [ ([KeyVal], v) ]
rows t = go (keyvals t) mempty
  where
    go [] spec = let spec' = reverse spec
                 in case Map.lookup spec' (contents t) of
                      Nothing -> mempty
                      Just v -> single (snd <$> spec', v)
    go ((key, vals):kvs) spec =
      concatMap (\v -> let spec' = (key,v):spec in go kvs spec') vals
