{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.OrdECM
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- A cache that holds values for a length of time that uses 'Ord' keys with 
-- "Data.Map.Strict".
-- 
-- An example of creating a cache for accessing files:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Caching.ExpiringCacheMap.OrdECM (newECMIO, lookupECM, CacheSettings(..), consistentDuration)
-- > 
-- > import qualified Data.Time.Clock.POSIX as POSIX (POSIXTime, getPOSIXTime)
-- > import qualified Data.ByteString.Char8 as BS
-- > import System.IO (withFile, IOMode(ReadMode))
-- > 
-- > example = do
-- >   filecache <- newECMIO
-- >         (consistentDuration 100 -- Duration between access and expiry time of each item
-- >           (\state id -> do BS.putStrLn "Reading a file again..."
-- >                            withFile (case id :: BS.ByteString of
-- >                                        "file1" -> "file1.txt"
-- >                                        "file2" -> "file2.txt")
-- >                               ReadMode $
-- >                               \fh -> do content <- BS.hGetContents fh
-- >                                         return $! (state, content)))
-- >         (do time <- POSIX.getPOSIXTime
-- >             return (round (time * 100)))
-- >         1 -- Time check frequency: (accumulator `mod` this_number) == 0.
-- >         (CacheWithLRUList
-- >           6     -- Expected size of key-value map when removing elements.
-- >           6     -- Size of map when to remove items from key-value map.
-- >           12    -- Size of list when to compact
-- >           )
-- >   
-- >   -- Use lookupECM whenever the contents of "file1" is needed.
-- >   b <- lookupECM filecache "file1"
-- >   BS.putStrLn b
-- >   return ()
-- > 
--

module Caching.ExpiringCacheMap.OrdECM (
    -- * Create cache
    newECMIO,
    newECMForM,
    consistentDuration,
    
    -- * Request value from cache
    lookupECM,
    
    -- * Value request function state
    getValReqState,
    
    -- * Invalidate cache
    invalidate,
    invalidateCache,
    
    -- * List keys
    keysCached,
    keysNotExpired,
    
    -- * Type
    ECM,
    CacheSettings(..)
) where

import qualified Control.Concurrent.MVar as MV
import qualified Data.Map.Strict as M
import qualified Data.List as L

import Caching.ExpiringCacheMap.Internal.Internal (updateUses, detECM, detNotExpired)
import Caching.ExpiringCacheMap.Types
import Caching.ExpiringCacheMap.Internal.Types

-- | Create a new expiring cache for retrieving uncached values via 'IO'
-- interaction (such as in the case of reading a file from disk), with
-- a shared state lock via an 'MV.MVar' to manage cache state.
--
-- Value request and time check request functions are provided as arguments.
--
-- The time check frequency value has to be 1 or higher, with higher values
-- postponing time checks for longer periods of time.
-- 
-- A cache setting specifies how the cache should remove entries when the
-- cache becomes a certain size. The only constructor for this is
-- 'CacheWithLRUList'.
--
newECMIO :: Ord k => (Maybe s -> k -> IO (TimeUnits, (Maybe s, v))) -> (IO TimeUnits)
  -> ECMIncr 
  -> CacheSettings
    -> IO (ECM IO MV.MVar s M.Map k v)
newECMIO retr gettime timecheckmodulo settings = do
  newECMForM retr gettime timecheckmodulo settings
    MV.newMVar MV.modifyMVar MV.readMVar

-- | Create a new expiring cache along arbitrary monads with provided
-- functions to create cache state in 'Monad' m2, and modify and read
-- cache state in 'Monad' m1.
--
-- 'newECMIO' is just a wrapper to this function with 'MV.MVar' functions:
--
-- @
--  newECMIO retr gettime timecheckmodulo cachesettings =
--    newECMForM retr gettime timecheckmodulo cachesettings
--      'MV.newMVar' 'MV.modifyMVar' 'MV.readMVar'
-- @
--
-- Value request and time check request functions are provided as arguments.
--
-- The time check frequency value has to be 1 or higher, with higher values
-- postponing time checks for longer periods of time.
-- 
-- A cache setting specifies how the cache should remove entries when the
-- cache becomes a certain size. The only constructor for this is
-- 'CacheWithLRUList'.
--
newECMForM :: (Monad m1, Monad m2) => Ord k => (Maybe s -> k -> m1 (TimeUnits, (Maybe s, v))) -> (m1 TimeUnits)
  -> ECMIncr
  -> CacheSettings
  -> ECMNewState m2 mv s M.Map k v
  -> ECMEnterState m1 mv s M.Map k v
  -> ECMReadState m1 mv s M.Map k v
    -> m2 (ECM m1 mv s M.Map k v)
newECMForM retr gettime timecheckmodulo (CacheWithLRUList minimumkeep removalsize compactlistsize)
           newstate enterstate readstate =
  if timecheckmodulo <= 0
    then error "Modulo time check must be 1 or higher."
    else do
      m'maps <- newstate $ CacheState ( Nothing, M.empty, 0, ([], 0), 0 )
      return $ ECM ( m'maps, retr, gettime, minimumkeep, timecheckmodulo, removalsize,
                     compactlistsize, enterstate, readstate )

-- | Request a value associated with a key from the cache.
--
--  * If the value is not in the cache, it will be requested through the 
--    function defined through 'newECM', its computation returned and the
--    value stored in the cache state map.
-- 
--  * If the value is in the cache and has not expired, it will be returned.
--
--  * If the value is in the cache and a new time is computed in the same
--    lookup, and the value has been determined to have since expired, it 
--    will be discarded and a new value will be requested for this computation.
--
-- Every 'lookupECM' computation increments an accumulator in the cache state 
-- which is used to keep track of the succession of key accesses. Based on the
-- parameters provided with the 'CacheWithLRUList' constructor, this history 
-- of key accesses is then used to remove entries from the cache back down to 
-- a minimum size. Also, when the modulo of the accumulator and the modulo 
-- value computes to 0, the time request function is invoked. In some cases
-- the accumulator may get incremented more than once in a 'lookupECM'
-- computation.
--
-- As the accumulator is a bound unsigned integer, when the accumulator
-- increments back to 0, the cache state is completely cleared.
-- 
-- The time request function is invoked in one of two different conditions
-- 
--  * When a new key-value entry is requested, the current time is also
--    requested during the same lookup, as a recent time determination is
--    needed for a new entry in the key-value cache.
-- 
--  * When the modulo of the accumulator and a specified value equals to 0.
--
-- When the current time is determined during a lookup, access times of the
-- entries in the key-value cache are compared with the new time to filter
-- out expired entries from the key-value map.
-- 
lookupECM :: (Monad m, Ord k) => ECM m mv s M.Map k v -> k -> m v
lookupECM ecm id = do
  enter m'maps $
    \(CacheState (retr_state, maps, mapsize, uses, incr)) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then lookupECM' (retr_state, M.empty, 0, ([], 0), 0) (0+1)
            else lookupECM' (retr_state, maps, mapsize, uses, incr) incr'
  where
    
    ECM ( m'maps, retr, gettime, minimumkeep, timecheckmodulo, removalsize,
          compactlistsize, enter, _ro ) = ecm
  
    -- Reversing the list first before turning into a map, so the higher value
    -- which is at the beginning will be at the end. And fromList retains the
    -- last value for a key in the list.
    mnub = M.toList . M.fromList . reverse 
    lookupECM' (retr_state, maps, mapsize, uses, incr) incr' = do
      let uses' = updateUses uses id incr' compactlistsize mnub
      (ret, do_again) <- det retr_state maps mapsize uses' incr'
      if do_again
        then do let (CacheState (retr_state', maps', mapsize', uses'', incr''), _) = ret
                    uses''' = updateUses uses'' id incr'' compactlistsize mnub
                (ret', _) <- det retr_state' maps' mapsize' uses''' incr''
                return ret'
        else return ret
    
    det retr_state maps mapsize uses' incr' =
      detECM (M.lookup id maps) retr_state (retr retr_state id)
        ( (\time_r -> M.insert id time_r maps),
          (\time_r keepuses -> M.insert id time_r $! M.intersection maps $ M.fromList keepuses),
          mnub, minimumkeep, removalsize )
        gettime
        M.filter
        mapsize M.size
        uses' incr' timecheckmodulo maps


getValReqState :: (Monad m, Ord k) => ECM m mv s M.Map k v -> k -> m (Maybe s)
getValReqState ecm id = do
  CacheState (retr_state, maps, mapsize, uses, incr) <- read m'maps
  return retr_state
  where
    ECM ( m'maps, _, _, _, _, _, _, _, read ) = ecm


-- | Invalidates a key from the cache and returns its value if any.
--   Note that this is a sequential composition of a read and modify of the
--   mutable cache container (e.g. 'MV.readMVar' followed by 'MV.modifyMVar'
--   with 'newECMIO' instances).
--
invalidate :: (Monad m, Ord k) => ECM m mv s M.Map k v -> k -> m (Maybe v)
invalidate ecm id = do
  CacheState (_, maps0, _, _, _) <- read m'maps
  case M.lookup id maps0 of
    Just time_prev0 -> do
      prev0' <- enter m'maps $
        \(CacheState (retr_state, maps, mapsize, uses, incr)) ->
          let (_, _, prev) =
                case M.lookup id maps of
                  Just time_prev -> time_prev
                  Nothing -> time_prev0
              maps' = M.delete id maps
           in return (CacheState (retr_state, maps', mapsize, uses, incr), prev)
      return $ Just prev0'
    Nothing -> return Nothing
  where
    ECM ( m'maps, _, _, _, _, _, compactlistsize, enter, read ) = ecm


-- | Invalidates the entire cache and returns the last key and value if any.
--   Note that this is a sequential composition of a read and modify of the
--   mutable cache container (e.g. 'MV.readMVar' followed by 'MV.modifyMVar'
--   with 'newECMIO' instances).
--
invalidateCache :: (Monad m, Ord k) => ECM m mv s M.Map k v -> m (Maybe (k, v))
invalidateCache ecm = do
  CacheState (_, maps0, _, (uses0, _), _) <- read m'maps
  case (M.toList $ M.intersection (M.fromList $ reverse uses0) maps0) of
    [] -> return Nothing
    uses0' -> 
      let (id, _) = L.maximumBy (\(_,a) (_,b) -> compare a b) uses0' in
      case M.lookup id maps0 of
        Just time_prev0 -> do
          prev0' <- enter m'maps $
            \(CacheState (retr_state, maps, _mapsize, _uses, _incr)) ->
              let (_, _, prev) =
                    case M.lookup id maps of
                      Just time_prev -> time_prev
                      Nothing -> time_prev0
               in return (CacheState (retr_state, M.empty, 0, ([], 0), 0), prev)
          return $ Just (id, prev0')
  where
    ECM ( m'maps, _, _, _, _, _, compactlistsize, enter, read ) = ecm


-- | List of keys in the cache map without performing a time check, returning
--   both stored keys that are expired and keys that are not expired. keys are
--   in an unspecified order.
--
keysCached :: (Monad m, Ord k) => ECM m mv s M.Map k v -> m [k]
keysCached ecm = do
  CacheState (_, maps0, _, _, _) <- read m'maps
  return $ M.keys maps0
  where
    ECM ( m'maps, _, _, _, _, _, _, _, read ) = ecm


-- | List of keys in the cache map that are not expired values. A time check
--   is always performed to compare with the elapsed time left with each key.
--   The cache state is not modified and the time check is not performed from
--   within a modifying state context, e.g. not within 'MV.modifyMVar' with a 
--   'newECMIO' instance. Keys are in an unspecified order. 
--
keysNotExpired :: (Monad m, Ord k) => ECM m mv s M.Map k v -> m [k]
keysNotExpired ecm = do
  CacheState (_, maps0, _, _, _) <- read m'maps
  current_time <- gettime
  return $ detNotExpired current_time $ M.toList maps0
  where
    ECM ( m'maps, _, gettime, _, _, _, _, _, read ) = ecm



{-

These functions would require inclusion of a enter_ function (like modifyMVar_)

putValReqState :: (Monad m, Ord k) => ECM m mv s M.Map k v -> k -> Maybe s -> m (Maybe s)
putValReqState ecm id new_state = do
  enter_ m'maps $
    \(CacheState (retr_state, maps, mapsize, uses, incr)) ->
      return (CacheState (new_state, maps, mapsize, uses, incr), retr_state)
  where
    
    ECM ( m'maps, _, _, _, _, _, _, _, enter_, _ro ) = ecm


clearCache :: (Monad m, Ord k) => ECM m mv s M.Map k v -> m ()
clearCache ecm = do
  enter_ m'maps $
    \(CacheState (retr_state, maps, mapsize, uses, incr)) ->
      return $ CacheState (retr_state, M.empty, 0, ([], 0), 0)
  where
    ECM ( m'maps, _, _, _, _, _, _, enter, enter_, _ ) = ecm

-}


{-
-- This function differs from 'lookupECM' only in the case that the value
-- being requested also causes a new time to have been computed during the 
-- same lookup, and have been found to be out of date. When the condition 
-- happens, this function returns the old cached value without attempting
-- to request a new value, despite being out of date. However, it does
-- clear the key from the key-value store for the next request.
--
lookupECMUse :: (Monad m, Ord k) => ECM m mv s M.Map k v -> k -> m v
lookupECMUse ecm id = do
  enter m'maps $
    \(CacheState (retr_state, maps, mapsize, uses, incr)) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then lookupECM' (retr_state, M.empty, 0, ([], 0), 0) (0+1)
            else lookupECM' (retr_state, maps, mapsize, uses, incr) incr'
  where
    
    ECM ( m'maps, retr, gettime, minimumkeep, timecheckmodulo, removalsize,
          compactlistsize, enter, _ro ) = ecm
  
    mnub = M.toList . M.fromList . reverse
    lookupECM' (retr_state, maps, mapsize, uses, incr) incr' = do
      let uses' = updateUses uses id incr' compactlistsize mnub
      (ret, _) <-
          detECM (M.lookup id maps) retr_state (retr retr_state id)
            ( (\time_r -> M.insert id time_r maps),
              (\time_r keepuses -> M.insert id time_r $! M.intersection maps $ M.fromList keepuses),
              mnub, minimumkeep, removalsize )
            gettime
            M.filter mapsize M.size
            uses' incr' timecheckmodulo maps
      return ret
-}


-- | Used with 'newECMIO' or 'newECMForM' to provide a consistent duration for requested values.
consistentDuration :: (Monad m, Ord k) => TimeUnits -> (Maybe s -> k -> m (Maybe s, v)) -> (Maybe s -> k -> m (TimeUnits, (Maybe s, v)))
consistentDuration duration fun =
  \state id -> do
    ret <- fun state id
    return (duration, ret)

