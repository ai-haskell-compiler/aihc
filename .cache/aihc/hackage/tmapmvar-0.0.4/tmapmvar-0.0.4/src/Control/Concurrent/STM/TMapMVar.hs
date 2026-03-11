module Control.Concurrent.STM.TMapMVar where

import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (void)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar', readTVar)
import Control.Concurrent.STM.TMVar (TMVar, readTMVar, tryReadTMVar, newEmptyTMVar, takeTMVar, tryTakeTMVar, swapTMVar, putTMVar)


putForceTMVar :: TMVar a -> a -> STM ()
putForceTMVar t x = do
  q <- tryReadTMVar t
  case q of
    Nothing -> putTMVar t x
    Just _  -> () <$ swapTMVar t x


newtype TMapMVar k a = TMapMVar
  { getTMapMVar :: TVar (Map k (TMVar a))
  }


newTMapMVar :: STM (TMapMVar k a)
newTMapMVar = TMapMVar <$> newTVar Map.empty

keys :: TMapMVar k a -> STM [k]
keys (TMapMVar m) = Map.keys <$> readTVar m

peekElems :: TMapMVar k a -> STM [a]
peekElems (TMapMVar m) = do
  m' <- readTVar m
  let ts = Map.elems m'
  catMaybes <$> traverse tryReadTMVar ts


-- | Blocks if it's full
insert :: (Ord k) => TMapMVar k a -> k -> a -> STM ()
insert t k a = do
  x <- getTMVar t k
  putTMVar x a

-- | Doesn't Block
insertForce :: (Ord k) => TMapMVar k a -> k -> a -> STM ()
insertForce t k a = do
  x <- getTMVar t k
  putForceTMVar x a


-- | Blocks, and deletes upon looking it up
lookup :: (Ord k) => TMapMVar k a -> k -> STM a
lookup t k = do
  x <- getTMVar t k
  takeTMVar x

tryLookup :: (Ord k) => TMapMVar k a -> k -> STM (Maybe a)
tryLookup t k = do
  x <- getTMVar t k
  tryTakeTMVar x

-- | Blocks, but doesn't delete when looking it up
observe :: (Ord k) => TMapMVar k a -> k -> STM a
observe t k = do
  x <- getTMVar t k
  readTMVar x

tryObserve :: (Ord k) => TMapMVar k a -> k -> STM (Maybe a)
tryObserve t k = do
  x <- getTMVar t k
  tryReadTMVar x

delete :: (Ord k) => TMapMVar k a -> k -> STM ()
delete t k =
  void $ tryLookup t k



-- * Utils

getTMVar :: (Ord k) => TMapMVar k a -> k -> STM (TMVar a)
getTMVar (TMapMVar m) k = do
  m' <- readTVar m
  case Map.lookup k m' of
    Nothing -> do
      t <- newEmptyTMVar
      modifyTVar' m (Map.insert k t)
      pure t
    Just t -> pure t
