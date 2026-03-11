
-- | Bijections via strict maps.

module Data.Bijection.Map
  ( module Data.Bijection.Class
  , S.Map
  ) where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Tuple (swap)
import           GHC.Generics
import qualified Data.Map.Strict as S

import           Data.Bijection.Class



instance (Eq d, Ord d) => DomCod (S.Map d c) where
  type Dom (S.Map d c) = d
  type Cod (S.Map d c) = c
  member h k = S.member k h
  lookup h k = S.lookup k h
  deleteDC h k = (,S.delete k h) <$> S.lookup k h
  insertDC h (d,c) = S.insert d c h
  toListDC = S.toList
  nullDC = S.null
  emptyDC = S.empty
  sizeDC = S.size
  fromListDC = S.fromList

