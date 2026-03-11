{-# LANGUAGE RecordWildCards #-}

module Network.Control.LRUCache (
    -- * LRU cache
    LRUCache,
    empty,
    insert,
    delete,
    lookup,
    lookup',

    -- * IO
    LRUCacheRef,
    newLRUCacheRef,
    cached,
    cached',
    setLRUCapacity,

    -- * Internal
    empty',
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Prelude hiding (lookup)

----------------------------------------------------------------

type Priority = Int64

-- | Sized cache based on least recently used.
data LRUCache k v = LRUCache
    { lcLimit :: Int
    -- ^ The maximum number of elements in the queue
    , lcTick :: Priority
    -- ^ The next logical time
    , lcQueue :: OrdPSQ k Priority v
    }
    deriving (Eq, Show)

----------------------------------------------------------------

-- | Empty 'LRUCache'. /O(1)/
empty
    :: Int
    -- ^ The size of 'LRUCache'.
    -> LRUCache k v
empty capacity =
    LRUCache
        { lcLimit = capacity
        , lcTick = 0
        , lcQueue = PSQ.empty
        }

-- | Empty 'LRUCache'. /O(1)/
empty'
    :: Int
    -- ^ The size of 'LRUCache'.
    -> Int64
    -- ^ Counter
    -> LRUCache k v
empty' capacity tick =
    LRUCache
        { lcLimit = capacity
        , lcTick = tick
        , lcQueue = PSQ.empty
        }

----------------------------------------------------------------

trim :: Ord k => LRUCache k v -> LRUCache k v
trim c@LRUCache{..}
    | lcTick == maxBound =
        let siz = fromIntegral $ PSQ.size lcQueue
            diff = (maxBound :: Priority) - siz
            psq = PSQ.unsafeMapMonotonic (\_ p v -> (p - diff, v)) lcQueue
         in LRUCache
                { lcLimit = lcLimit
                , lcTick = siz
                , lcQueue = psq
                }
    | PSQ.size lcQueue > lcLimit = c{lcQueue = PSQ.deleteMin lcQueue}
    | otherwise = c

----------------------------------------------------------------

-- | Inserting. /O(log n)/
insert :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
insert key val c@LRUCache{..} = trim c'
  where
    queue = PSQ.insert key lcTick val lcQueue
    c' = c{lcTick = lcTick + 1, lcQueue = queue}

----------------------------------------------------------------

-- | Deleting. /O(log n)/
delete :: Ord k => k -> LRUCache k v -> LRUCache k v
delete k c@LRUCache{..} = c{lcQueue = q}
  where
    q = PSQ.delete k lcQueue

----------------------------------------------------------------

-- | Looking up. /O(log n)/
lookup :: Ord k => k -> LRUCache k v -> Maybe v
lookup k LRUCache{..} = snd <$> PSQ.lookup k lcQueue

-- | Looking up and changing priority. /O(log n)/
lookup' :: Ord k => k -> LRUCache k v -> Maybe (v, LRUCache k v)
lookup' k c@LRUCache{..} = case PSQ.alter lookupAndBump k lcQueue of
    (Nothing, _) -> Nothing
    (Just v, q) ->
        let c' = trim $ c{lcTick = lcTick + 1, lcQueue = q}
         in Just (v, c')
  where
    lookupAndBump Nothing = (Nothing, Nothing)
    -- setting its priority to lcTick
    lookupAndBump (Just (_p, v)) = (Just v, Just (lcTick, v))

----------------------------------------------------------------

-- | Mutable LRUCache.
newtype LRUCacheRef k v = LRUCacheRef (IORef (LRUCache k v))

-- | Creating 'LRUCacheRef'.
newLRUCacheRef :: Int -> IO (LRUCacheRef k v)
newLRUCacheRef capacity = LRUCacheRef <$> newIORef (empty capacity)

-- | Looking up a target and adjusting the LRU cache.
--   If not found, a new value is inserted.
--   A pair of value and "found" is returned.
cached :: Ord k => LRUCacheRef k v -> k -> IO v -> IO (v, Bool)
cached (LRUCacheRef ref) k io = do
    lookupRes <- atomicModifyIORef' ref $ \c -> case lookup' k c of
        Nothing -> (c, Nothing)
        Just (v, c') -> (c', Just v)
    case lookupRes of
        Just v -> return (v, True)
        Nothing -> do
            v <- io
            atomicModifyIORef' ref $ \c -> (insert k v c, ())
            return (v, False)

-- | Looking up a target and adjusting the LRU cache.
cached' :: Ord k => LRUCacheRef k v -> k -> IO (Maybe v)
cached' (LRUCacheRef ref) k = do
    atomicModifyIORef' ref $ \c -> case lookup' k c of
        Nothing -> (c, Nothing)
        Just (v, c') -> (c', Just v)

-- | Setting capacity of the LRU cache.
setLRUCapacity :: LRUCacheRef k v -> Int -> IO ()
setLRUCapacity (LRUCacheRef ref) lim = atomicModifyIORef' ref $ \c ->
    (c{lcLimit = lim}, ())
