module Data.HSet.Mutable
  ( HKey
  , HSet
  , new
  , insert
  , lookup
  , delete
  ) where

import Data.HSet.Types

import Prelude hiding (lookup, length)
import Data.Maybe (fromMaybe)

import Data.Typeable (typeOf, typeRepFingerprint)
import GHC.Fingerprint
import Data.Dynamic

import           Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.ST



data HSet s = HSet
  { hSetValues :: {-# UNPACK #-} !(HashTable s HKey' Dynamic)
  , hSetCount  :: {-# UNPACK #-} !(HashTable s Fingerprint Int)
  }


new :: ST s (HSet s)
new = HSet <$> HT.new <*> HT.new


insert :: Typeable a => a -> HSet s -> ST s (HKey a)
insert x (HSet xs count) = do
  let f = typeRepFingerprint $ typeOf x
  c <- fromMaybe 0 <$> HT.lookup count f
  HT.insert count f (c+1)
  let k = HKey' f c
  HT.insert xs k (toDyn x)
  pure (HKey k)


lookup :: Typeable a => HKey a -> HSet s -> ST s (Maybe a)
lookup (HKey k) (HSet xs _) = (>>= fromDynamic) <$> HT.lookup xs k


delete :: HKey a -> HSet s -> ST s ()
delete (HKey k@(HKey' f _)) (HSet xs count) = do
  HT.delete count f
  HT.delete xs k
