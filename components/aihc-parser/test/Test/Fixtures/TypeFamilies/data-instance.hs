{- ORACLE_TEST
id: data-instance
category: declarations
expected: xfail
reason: data instance
-}
{-# LANGUAGE TypeFamilies #-}
module DataInstance where

data family GMap k v
data instance GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)
