module XmlParser.TupleHashMap
  ( TupleHashMap,
    KeyConstraints,
    empty,
    insertSemigroup,
    alterF,
    toList,
  )
where

import qualified Data.HashMap.Strict as HashMap
import XmlParser.Prelude hiding (empty, fromList, toList)

newtype TupleHashMap k1 k2 v = TupleHashMap (HashMap k1 (HashMap k2 v))

-- |
-- Serves to reduce noise in signatures.
type KeyConstraints k1 k2 = (Eq k1, Hashable k1, Eq k2, Hashable k2)

empty :: TupleHashMap k1 k2 v
empty =
  TupleHashMap HashMap.empty

insertSemigroup :: (Semigroup v, KeyConstraints k1 k2) => k1 -> k2 -> v -> TupleHashMap k1 k2 v -> TupleHashMap k1 k2 v
insertSemigroup k1 k2 v (TupleHashMap map1) =
  HashMap.alter
    ( maybe
        (Just (HashMap.singleton k2 v))
        (Just . HashMap.insertWith (<>) k2 v)
    )
    k1
    map1
    & TupleHashMap

alterF :: (Functor f, KeyConstraints k1 k2) => (Maybe v -> f (Maybe v)) -> k1 -> k2 -> TupleHashMap k1 k2 v -> f (TupleHashMap k1 k2 v)
alterF fn k1 k2 (TupleHashMap map1) =
  HashMap.alterF
    ( \case
        Just map2 ->
          HashMap.alterF fn k2 map2
            & fmap (\map2 -> if HashMap.null map2 then Nothing else Just map2)
        Nothing ->
          fn Nothing
            & (fmap . fmap) (HashMap.singleton k2)
    )
    k1
    map1
    & fmap TupleHashMap

toList :: TupleHashMap k1 k2 b -> [(k1, k2, b)]
toList (TupleHashMap map1) =
  do
    (k1, map2) <- HashMap.toList map1
    (k2, v) <- HashMap.toList map2
    return (k1, k2, v)
