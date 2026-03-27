{-# LANGUAGE TypeFamilies #-}
module AssociatedData where

class GMapKey k where
  data GMap k :: * -> *
