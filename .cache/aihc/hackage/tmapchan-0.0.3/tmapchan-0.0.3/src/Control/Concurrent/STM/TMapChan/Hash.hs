module Control.Concurrent.STM.TMapChan.Hash where

import Prelude hiding (lookup)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Control.Monad (void, forM_)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan, readTChan, tryReadTChan, peekTChan, tryPeekTChan, unGetTChan)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar')


newtype TMapChan k a = TMapChan {runTMapChan :: TVar (HashMap k (TChan a))}


newTMapChan :: STM (TMapChan k a)
newTMapChan = TMapChan <$> newTVar HashMap.empty

insert :: (Eq k, Hashable k) => TMapChan k a -> k -> a -> STM ()
insert t k a = do
  c <- getTChan t k
  writeTChan c a

insertMany :: (Eq k, Hashable k) => TMapChan k a -> k -> [a] -> STM ()
insertMany t k as = forM_ as (insert t k)

-- | Inserts the element to the head of the stack, to be viewed next
insertFirst :: (Eq k, Hashable k) => TMapChan k a -> k -> a -> STM ()
insertFirst t k a = do
  c <- getTChan t k
  unGetTChan c a

-- | Blocks until there's a value available to remove from the mutlimap
lookup :: (Eq k, Hashable k) => TMapChan k a -> k -> STM a
lookup t k = do
  c <- getTChan t k
  readTChan c

tryLookup :: (Eq k, Hashable k) => TMapChan k a -> k -> STM (Maybe a)
tryLookup t k = do
  c <- getTChan t k
  tryReadTChan c

lookupAll :: (Eq k, Hashable k) => TMapChan k a -> k -> STM [a]
lookupAll t k = do
  mNext <- tryLookup t k
  case mNext of
    Nothing -> pure []
    Just next -> (next:) <$> lookupAll t k

-- | Blocks until there's a vale available to view, without removing it
observe :: (Eq k, Hashable k) => TMapChan k a -> k -> STM a
observe t k = do
  c <- getTChan t k
  peekTChan c

tryObserve :: (Eq k, Hashable k) => TMapChan k a -> k -> STM (Maybe a)
tryObserve t k = do
  c <- getTChan t k
  tryPeekTChan c

observeAll :: (Eq k, Hashable k) => TMapChan k a -> k -> STM [a]
observeAll t k = do
  mNext <- tryObserve t k
  case mNext of
    Nothing -> pure []
    Just next -> (:) next <$> observeAll t k


-- | Deletes the /next/ element in the map, if it exists. Doesn't block.
delete :: (Eq k, Hashable k) => TMapChan k a -> k -> STM ()
delete t k = void (tryLookup t k)

-- | Clears the queue at the key
deleteAll :: (Eq k, Hashable k) => TMapChan k a -> k -> STM ()
deleteAll t k = void (lookupAll t k)


-- * Utils

keys :: TMapChan k a -> STM [k]
keys (TMapChan m) = HashMap.keys <$> readTVar m


-- | Creates a new one if it doesn't already exist
getTChan :: (Eq k, Hashable k) => TMapChan k a -> k -> STM (TChan a)
getTChan (TMapChan t) k = do
  xs <- readTVar t
  case HashMap.lookup k xs of
    Nothing -> do
      c' <- newTChan
      writeTVar t (HashMap.insert k c' xs)
      pure c'
    Just c' -> pure c'

setTChan :: (Eq k, Hashable k) => TMapChan k a -> k -> TChan a -> STM ()
setTChan (TMapChan t) k c = modifyTVar' t (HashMap.insert k c)


broadcast :: (Eq k, Hashable k) => TMapChan k a -> a -> STM ()
broadcast t@(TMapChan xs) a = do
  ks <- HashMap.keys <$> readTVar xs
  forM_ ks (\k -> insert t k a)


cloneAt :: (Eq k, Hashable k)
        => TMapChan k a
        -> k -- ^ key to clone /from/
        -> k -- ^ key to clone /to/
        -> STM ()
cloneAt t@(TMapChan xs) kFrom kTo = do
  as <- observeAll t kFrom
  c <- newTChan
  forM_ as (writeTChan c)
  modifyTVar' xs (HashMap.insert kTo c)

-- | Clones all the content for every key, by the key.
cloneAll :: (Eq k, Hashable k) => TMapChan k a -> k -> STM ()
cloneAll t@(TMapChan xs) k = do
  as <- readTVar xs
  c <- newTChan
  forM_ (HashMap.keys as) $ \k' -> do
    as' <- observeAll t k'
    forM_ as' (writeTChan c)
  modifyTVar' xs (HashMap.insert k c)

-- | Clones all the content from every channel, and inserts the unique subset of them to `k`, in an unspecified order.
cloneAllUniquely :: ( Eq k, Hashable k
                    , Eq a, Hashable a
                    )
                  => TMapChan k a
                  -> k
                  -> STM ()
cloneAllUniquely t@(TMapChan xs) k = do
  as <- readTVar xs
  c <- newTChan
  as' <- newTVar HashSet.empty
  forM_ (HashMap.keys as) $ \k' -> do
    as'More <- HashSet.fromList <$> observeAll t k'
    modifyTVar' as' (HashSet.union as'More)
  as'All <- HashSet.toList <$> readTVar as'
  forM_ as'All (writeTChan c)
  modifyTVar' xs (HashMap.insert k c)
