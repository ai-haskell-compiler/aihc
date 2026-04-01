{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module MultipleAssociatedInstances where

class GMapKey k where
  data GMap k v

data Flob

instance GMapKey Flob where
  data GMap Flob [v] = G1 v
  data GMap Flob Int = G2 Int