{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.Internal
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- A module with internal functions used in common by HashECM and OrdECM.
-- Assume these functions to change from version to version.
-- 

module Caching.ExpiringCacheMap.Internal.Internal (
    updateUses,
    detECM,
    getStatsString,
    detNotExpired
) where

import qualified Data.List as L

import Caching.ExpiringCacheMap.Types
import Caching.ExpiringCacheMap.Internal.Types

updateUses :: (Eq k) => ([(k, ECMIncr)], ECMULength) -> k
  -> ECMIncr -> ECMULength -> ([(k, ECMIncr)] -> [(k, ECMIncr)])
    -> ([(k, ECMIncr)], ECMULength)
{-# INLINE updateUses #-}
updateUses (usesl, lcount) id incr' compactlistsize compactUses
 | lcount >= 5 =
    case usesl of
        (id', _) : rest | id' == id ->
          ((id', incr') : rest, lcount)

        latest : (id1, oincr1) : (id2, oincr2) : (id3, oincr3) : (id4, oincr4) : rest ->
          case True of
            _ | id1 == id -> ((id1, incr') : latest : (id2, oincr2) : (id3, oincr3) : (id4, oincr4) : rest, lcount)
            _ | id2 == id -> ((id2, incr') : latest : (id1, oincr1) : (id3, oincr3) : (id4, oincr4) : rest, lcount)
            _ | id3 == id -> ((id3, incr') : latest : (id1, oincr1) : (id2, oincr2) : (id4, oincr4) : rest, lcount)
            _ | id4 == id -> ((id4, incr') : latest : (id1, oincr1) : (id2, oincr2) : (id3, oincr3) : rest, lcount)
            _ -> justPrepend
          {-
          if id1 == id
            then ((id1, incr') : latest : (id2, oincr2) : (id3, oincr3) : (id4, oincr4) : rest, lcount)
            else if id2 == id 
                   then ((id2, incr') : latest : (id1, oincr1) : (id3, oincr3) : (id4, oincr4) : rest, lcount)
                   else if id3 == id 
                          then ((id3, incr') : latest : (id1, oincr1) : (id2, oincr2) : (id4, oincr4) : rest, lcount)
                          else if id4 == id 
                                 then ((id4, incr') : latest : (id1, oincr1) : (id2, oincr2) : (id3, oincr3) : rest, lcount)
                                 else justPrepend
          -}

        _ -> justPrepend
 | otherwise =
    case usesl of
        (id', _) : rest | id' == id ->
          ((id', incr') : rest, lcount)

        latest : (id1, oincr1) : (id2, oincr2) : (id3, oincr3) : rest ->
          case True of
            _ | id1 == id -> ((id1, incr') : latest : (id2, oincr2) : (id3, oincr3) : rest, lcount)
            _ | id2 == id -> ((id2, incr') : latest : (id1, oincr1) : (id3, oincr3) : rest, lcount)
            _ | id3 == id -> ((id3, incr') : latest : (id1, oincr1) : (id2, oincr2) : rest, lcount)
            _ -> justPrepend
          {-
          if id1 == id
            then ((id1, incr') : latest : (id2, oincr2) : (id3, oincr3) : rest, lcount)
            else if id2 == id 
                   then ((id2, incr') : latest : (id1, oincr1) : (id3, oincr3) : rest, lcount)
                   else if id3 == id 
                          then ((id3, incr') : latest : (id1, oincr1) : (id2, oincr2) : rest, lcount)
                          else justPrepend
          -}
        
        latest : (id1, oincr1) : (id2, oincr2) : rest ->
          case True of
            _ | id1 == id -> ((id1, incr') : latest : (id2, oincr2) : rest, lcount)
            _ | id2 == id -> ((id2, incr') : latest : (id1, oincr1) : rest, lcount)
            _ -> justPrepend
          {-
          if id1 == id
            then ((id1, incr') : latest : (id2, oincr2) : rest, lcount)
            else if id2 == id 
                   then ((id2, incr') : latest : (id1, oincr1) : rest, lcount)
                   else justPrepend
          -}
        
        latest : (id', _) : rest ->
          if id' == id 
            then ((id', incr') : latest : rest, lcount)
            else justPrepend
         
        _ -> justPrepend
  where
    justPrepend =
      if lcount > compactlistsize
        then let newusesl = compactUses usesl
              in ((id, incr') : newusesl, (+1) $! (L.length newusesl) )
        else ((id, incr') : usesl, lcount + 1)

detECM
  :: (Monad m, Eq k) =>
     Maybe (TimeUnits, TimeUnits, v)
     -> Maybe s
     -> m (TimeUnits, (Maybe s, v))
     -> ( ((TimeUnits, TimeUnits, v) -> mp k (TimeUnits, TimeUnits, v)),
          ((TimeUnits, TimeUnits, v) -> [(k, ECMIncr)] -> mp k (TimeUnits, TimeUnits, v)),
          ([(k, ECMIncr)] -> [(k, ECMIncr)]),
          ECMMapSize,
          ECMULength)
     -> m TimeUnits
     -> (((TimeUnits, TimeUnits, v) -> Bool)
         -> mp k (TimeUnits, TimeUnits, v) -> mp k (TimeUnits, TimeUnits, v))
     -> ECMMapSize
     -> (mp k (TimeUnits, TimeUnits, v) -> ECMMapSize)
     -> ([(k, ECMIncr)], ECMULength)
     -> ECMIncr
     -> ECMIncr
     -> mp k (TimeUnits, TimeUnits, v)
       -> m ((CacheState s mp k v, v), Bool)
{-# INLINE detECM #-}
detECM result retr_state retr_id etc  gettime filt  cmapsize newsize uses' incr' timecheckmodulo maps = 
    case result of
        Nothing -> do
          (expirytime, (retr_state', r)) <- retr_id
          time <- gettime
          let (newmaps,mapsize',newuses) = insertAndPerhapsRemoveSome etc cmapsize newsize filt time r expirytime uses'
          return $! ((CacheState (retr_state', newmaps, mapsize', newuses, incr'), r), False)
        Just (_accesstime, _expirytime, m) -> do
          if incr' `mod` timecheckmodulo == 0
            then do
              time <- gettime
              return $! let maps' = filterExpired time maps
                         in ((CacheState (retr_state, maps', (+0) $! newsize maps', uses', incr'), m), True)
            else return ((CacheState (retr_state, maps, cmapsize, uses', incr'), m), False)
  where
    filterExpired = filterExpired' filt
  

{-# INLINE insertAndPerhapsRemoveSome #-}  
insertAndPerhapsRemoveSome (insert_id1, insert_id2, mnub, minimumkeep, removalsize) cmapsize newsize filt time r expirytime uses =
      if cmapsize >= removalsize
        then 
          let (keepuses, _removekeys) = getKeepAndRemove usesl
              newmaps = insert_id2 (time, expirytime, r) keepuses
              newmaps' = filterExpired time newmaps
           in (newmaps', (+0) $! newsize newmaps', (keepuses, (+0) $! (L.length keepuses)))
        else
          let newmaps = insert_id1 (time, expirytime, r)
           in (newmaps, cmapsize + 1, uses) -- filterExpired time
      where
        (usesl, _lcount) = uses
        getKeepAndRemove =
          finalTup . splitAt minimumkeep . reverse . 
              sortI . map swap2 . mnub
            where swap2 (a,b) = (b,a)
                  finalTup (l1,l2) = 
                    (map (\(c,k) -> (k,c)) l1, map (\(c,k) -> k) l2)
                  sortI = L.sortBy (\(l,_) (r,_) -> compare l r)
    

        filterExpired = filterExpired' filt

{-# INLINE filterExpired' #-}
filterExpired' filt time =
      filt (\(accesstime, expirytime, _value) ->
                 (accesstime <= time) &&
                   (accesstime > (time - expirytime)))


detNotExpired
 :: TimeUnits -> [(k, (TimeUnits, TimeUnits, v))] -> [k]
{-# INLINE detNotExpired #-}
detNotExpired _time l = detNotExpired' _time l []

{-# INLINE detNotExpired' #-}
detNotExpired' _time [] l = reverse l
detNotExpired'  time ((key, (accesstime, expirytime, _value)) : r) l
  | (accesstime <= time) && (accesstime > (time - expirytime)) =
        detNotExpired' time r (key:l)
  | otherwise =
        detNotExpired' time r l


-- | Debugging function
--
getStatsString ecm = do
  CacheState (_retr_state, _maps, _mapsize, uses, _incr) <- ro m'uses
  return $ show uses
  where
    ECM ( m'uses, _retr, _gettime, _minimumkeep, _timecheckmodulo, _removalsize,
          _compactlistsize, _enter, ro ) = ecm



