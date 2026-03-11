{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Binary.Instances.UnorderedContainers where

import Data.Binary         (Binary, get, put)
import Data.Binary.Orphans ()

import qualified Data.Hashable     as Hashable
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

instance  (Hashable.Hashable k, Eq k, Binary k, Binary v) => Binary (HM.HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList

instance (Hashable.Hashable v, Eq v, Binary v) => Binary (HS.HashSet v) where
    get = fmap HS.fromList get
    put = put . HS.toList
