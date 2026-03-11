module Data.PredSet.Mutable
  ( PredSet
  , PredKey
  , new
  , insert
  , lookup
  ) where

import Prelude hiding (lookup)
import Data.Monoid

import           Data.HSet.Mutable (HSet, HKey)
import qualified Data.HSet.Mutable as HS
import           Data.HashTable.ST.Cuckoo (HashTable)
import qualified Data.HashTable.ST.Cuckoo as HT
import Data.Typeable
import Data.Hashable
import Control.Monad.ST



data CachedPred s k a = CachedPred
  { cachedPredPred  :: !(k -> Maybe a)
  , cachedPredCache :: {-# UNPACK #-} !(HashTable s k a)
  }

newCachedPred :: (k -> Maybe a)
              -> ST s (CachedPred s k a)
newCachedPred pred' = CachedPred pred' <$> HT.new

query :: ( Eq k
         , Hashable k
         ) => k
           -> CachedPred s k a
           -> ST s (Maybe a)
query k (CachedPred pred' cache) =
      (\mx -> getFirst $! First mx <> First (pred' k))
  <$> HT.lookup cache k


newtype PredSet s k = PredSet
  { getPredSet :: HSet s
  }

newtype PredKey s k a = PredKey
  { getPredKey :: HKey (CachedPred s k a)
  }


new :: ST s (PredSet s k)
new = PredSet <$> HS.new

insert :: ( Typeable k
          , Typeable a
          , Typeable s
          ) => (k -> Maybe a)
            -> PredSet s k
            -> ST s (PredKey s k a)
insert pred' (PredSet xs) = do
  cache <- newCachedPred pred'
  PredKey <$> HS.insert cache xs

lookup :: ( Eq k
          , Hashable k
          , Typeable s
          , Typeable k
          , Typeable a
          ) => PredKey s k a
            -> k
            -> PredSet s k
            -> ST s (Maybe a)
lookup (PredKey i) k (PredSet xs) = do
  mCachedPred <- HS.lookup i xs
  case mCachedPred of
    Nothing    -> pure Nothing
    Just cache -> query k cache
