
-- | A bijection between boxed, immutable vectors.

module Data.Bijection.Vector.Storable
  ( module Data.Bijection.Class
  , Vector
  ) where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Tuple (swap)
import           Data.Vector.Binary
import           Data.Vector.Serialize
import           Data.Vector.Storable (Vector, Storable)
import           Foreign.Storable.Tuple
import           GHC.Generics
import qualified Data.Vector.Generic as G

import           Data.Bijection.Class


instance (Storable c) => DomCod (Vector c) where
  type Dom (Vector c) = Int
  type Cod (Vector c) = c
  member v k = k >= 0 && k < G.length v
  lookup v k = v G.!? k
  deleteDC v k
    | k+1 == G.length v = Just (v G.! k, G.init v)
    | otherwise         = error "tried to delete non-last element"
  insertDC v (d,c)
    | d == G.length v          = G.snoc v c
    | d >= 0 && d < G.length v = v G.// [(d,c)]
    | otherwise                = error "tried to insert into non-contiguous range"
  toListDC = G.toList . G.indexed
  nullDC = G.null
  emptyDC = G.empty
  sizeDC = G.length
  fromListDC = G.fromList . map snd

