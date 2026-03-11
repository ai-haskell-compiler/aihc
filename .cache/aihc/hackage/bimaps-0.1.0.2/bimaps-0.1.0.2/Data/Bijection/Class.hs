
module Data.Bijection.Class where

import Control.Applicative ((<$>))
import Control.DeepSeq
import Data.Aeson
import Data.Binary
import Data.Serialize
import Data.Tuple (swap)
import GHC.Generics
import Prelude (Bool,Maybe,map,($),Int, maybe, id, (.), seq, Read, Show, Eq, return)
import Data.List (foldl')



-- | Bijection between finite sets.
--
-- Both data types are strict here.

data Bimap l r = Bimap !l !r
  deriving (Read,Show,Eq,Generic)

class DomCod z where
  type Dom z  :: *
  type Cod z  :: *
  member :: z -> Dom z -> Bool
  lookup :: z -> Dom z -> Maybe (Cod z)
  deleteDC :: z -> Dom z -> Maybe (Cod z, z)
  insertDC :: z -> (Dom z,Cod z) -> z
  toListDC :: z -> [(Dom z, Cod z)]
  nullDC :: z -> Bool
  emptyDC :: z
  sizeDC :: z -> Int
  fromListDC :: [(Dom z, Cod z)] -> z

instance (NFData l, NFData r) => NFData (Bimap l r) where
  rnf (Bimap l r) = rnf l `seq` rnf r `seq` ()

instance (Binary l, Binary r) => Binary (Bimap l r)
instance (Serialize l, Serialize r) => Serialize (Bimap l r)
instance (DomCodCnt l r, ToJSON (Dom l), ToJSON (Dom r)) => ToJSON (Bimap l r) where
  toJSON = toJSON . toList
instance (DomCodCnt l r, FromJSON (Dom l), FromJSON (Dom r)) => FromJSON (Bimap l r) where
  parseJSON j = fromList <$> parseJSON j

type DomCodCnt l r = (DomCod l, DomCod r, Dom l ~ Cod r, Dom r ~ Cod l)



contL :: Bimap l r -> l
contL (Bimap l r) = l

contR :: Bimap l r -> r
contR (Bimap l r) = r

memberL :: (DomCod l) => Bimap l r -> Dom l -> Bool
memberL (Bimap l r) e = member l e

memberR :: (DomCod r) => Bimap l r -> Dom r -> Bool
memberR (Bimap l r) e = member r e

lookupL :: (DomCod l) => Bimap l r -> Dom l -> Maybe (Cod l)
lookupL (Bimap l r) k = lookup l k

lookupR :: (DomCod r) => Bimap l r -> Dom r -> Maybe (Cod r)
lookupR (Bimap l r) k = lookup r k

empty :: (DomCodCnt l r) => Bimap l r
empty = Bimap emptyDC emptyDC

null :: DomCod l => Bimap l r -> Bool
null (Bimap l r) = nullDC l

size :: DomCod l => Bimap l r -> Int
size (Bimap l r) = sizeDC l

-- | Given a list of pairs @[(x,y)]@, turn it into a bimap @(x->y, y->x)@.

fromList :: DomCodCnt l r => [(Dom l, Dom r)] -> Bimap l r
fromList = foldl' insert empty

toList :: DomCodCnt l r => Bimap l r -> [(Dom l, Dom r)]
toList (Bimap l r) = toListDC l

insert :: (DomCodCnt l r) => Bimap l r -> (Dom l, Cod l) -> Bimap l r
insert (Bimap l r) (u,v) = Bimap (insertDC l (u,v)) (insertDC r (v,u))
{-# Inline insert #-}

deleteByL :: DomCodCnt l r => Bimap l r -> Dom l -> Bimap l r
deleteByL b@(Bimap l r) k = maybe b id $ do
  (k',l') <- deleteDC l k
  (_ ,r') <- deleteDC r k'
  return $ Bimap l' r'
{-# Inline deleteByL #-}

deleteByR :: DomCodCnt l r => Bimap l r -> Dom r -> Bimap l r
deleteByR b@(Bimap l r) k = maybe b id $ do
  (k',r') <- deleteDC r k
  (_ ,l') <- deleteDC l k'
  return $ Bimap l' r'
{-# Inline deleteByR #-}

findWithDefaultL :: DomCodCnt l r => Cod l -> Bimap l r -> Dom l -> Cod l
findWithDefaultL def = (maybe def id . ) . lookupL
{-# INLINE findWithDefaultL #-}

findWithDefaultR :: DomCodCnt l r => Cod r -> Bimap l r -> Dom r -> Cod r
findWithDefaultR def = (maybe def id . ) . lookupR
{-# INLINE findWithDefaultR #-}

