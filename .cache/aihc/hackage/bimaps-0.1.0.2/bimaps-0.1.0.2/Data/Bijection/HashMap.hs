
module Data.Bijection.HashMap
  ( module Data.Bijection.Class
  , H.HashMap
  , BimapHashMap
  ) where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Hashable (Hashable)
import           Data.Serialize
import           Data.Tuple (swap)
import           GHC.Generics
import qualified Data.HashMap.Strict as H

import           Data.Bijection.Class



type BimapHashMap d c = Bimap (H.HashMap d c) (H.HashMap c d)

instance (Eq d, Hashable d) => DomCod (H.HashMap d c) where
  type Dom (H.HashMap d c) = d
  type Cod (H.HashMap d c) = c
  member h k = H.member k h
  lookup h k = H.lookup k h
  deleteDC h k = (,H.delete k h) <$> H.lookup k h
  insertDC h (d,c) = H.insert d c h
  toListDC = H.toList
  nullDC = H.null
  emptyDC = H.empty
  sizeDC = H.size
  fromListDC = H.fromList

