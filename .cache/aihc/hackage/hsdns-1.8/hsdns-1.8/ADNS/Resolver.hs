{- |
   Module      :  ADNS.Resolver
   Copyright   :  (c) 2008 Peter Simons
   License     :  LGPL

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module implements a Haskell DNS Resolver on top of the
   ADNS library. GHC users should compile their code using the
   @-threaded@ runtime system.
 -}

module ADNS.Resolver
  ( Resolver
  , initResolver
  , toPTR
  , resolveA, resolvePTR, resolveMX, resolveSRV
  , query
  , dummyDNS
  )
  where

import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar
import Control.Monad      ( when )
import Data.List          ( sortBy )
import Data.Map           ( Map )
import qualified Data.Map as Map
import Network.Socket     ( HostName, HostAddress, PortNumber, hostAddressToTuple )
import ADNS.Base

-- |A 'Resolver' is an 'IO' computation which -- given the name
-- and type of the record to query -- returns an 'MVar' that will
-- eventually contain the 'Answer' from the Domain Name System.

type Resolver = String -> RRType -> [QueryFlag] -> IO (MVar Answer)

-- |Run the given 'IO' computation with an Initialized
-- 'Resolver'. Note that resolver functions can be shared,
-- and /should/ be shared between any number of 'IO'
-- threads. You may use multiple resolvers, of course, but
-- doing so defeats the purpose of an asynchronous resolver.

initResolver :: [InitFlag] -> (Resolver -> IO a) -> IO a
initResolver flags f =
  adnsInit flags $ \dns ->
    newMVar (RState dns Map.empty) >>= f . resolve

-- |Resolve a hostname's 'A' records.

resolveA :: Resolver -> HostName -> IO (Either Status [HostAddress])
resolveA resolver x = do
  Answer rc _ _ _ rs  <- resolver x A [] >>= takeMVar
  return (if rc /= sOK then Left rc else Right [addr | RRA (RRAddr addr) <- rs])

-- |Resolve a hostname's 'SRV' records.

resolveSRV :: Resolver -> HostName -> IO (Either Status [(HostName, PortNumber)])
resolveSRV resolver x = do
  Answer rc _ _ _ rs  <- resolver x SRV [] >>= takeMVar
  if rc /= sOK
     then return (Left rc)
     else do
       let cmp (RRSRV p1 _ _ _) (RRSRV p2 _ _ _) = compare p1 p2
           cmp _ _ = error $ showString "unexpected record in SRV lookup: " (show rs)
           rs' = sortBy cmp rs
           as = [ (host, toEnum port) | (RRSRV _ _ port host) <- rs' ]
       return (Right as)

-- |Get the 'PTR' records assigned to a host address. Note
-- that although the API allows for a record to have more
-- than one 'PTR' entry, this will actually not happen
-- because the GNU adns library can't handle this case and
-- will return 'sINCONSISTENT'.

resolvePTR :: Resolver -> HostAddress -> IO (Either Status [HostName])
resolvePTR resolver x = do
  Answer rc _ _ _ rs  <- resolver (toPTR x) PTR [] >>= takeMVar
  return (if rc /= sOK then Left rc else Right [ addr | RRPTR addr <- rs ])

-- |Resolve the mail exchangers for a hostname. The returned
-- list may contain more than one entry per hostname, in
-- case the host has several 'A' records. The records are
-- returned in the order you should try to contact them as
-- determined by the priority in the 'RRMX' response.

resolveMX :: Resolver -> HostName -> IO (Either Status [(HostName, HostAddress)])
resolveMX resolver x = do
  Answer rc _ _ _ rs  <- resolver x MX [] >>= takeMVar
  if rc /= sOK
     then return (Left rc)
     else do
       let cmp (RRMX p1 _) (RRMX p2 _) = compare p1 p2
           cmp _ _= error $ showString "unexpected record in MX lookup: " (show rs)
           rs' = sortBy cmp rs
           as   = [ (hn,a) | RRMX _ (RRHostAddr hn stat has) <- rs'
                           , stat == sOK && not (null has)
                           , RRAddr a <- has ]
       return (Right as)

-- |Convenience wrapper that will modify any of the
-- @revolveXXX@ functions above to return 'Maybe' rather
-- than 'Either'. The idea is that @Nothing@ signifies any
-- sort of failure: @Just []@ signifies 'sNXDOMAIN' or
-- 'sNODATA', and everything else signifies 'sOK'.
--
-- So if you aren't interested in getting accurate 'Status'
-- codes in case of failures. Wrap your DNS queries as
-- follows:
--
-- > queryA :: Resolver -> HostName -> IO (Maybe [HostAddress])
-- > queryA = query resolveA

query :: (Resolver -> a -> IO (Either Status [b]))
      -> Resolver -> a -> IO (Maybe [b])
query f dns x = fmap toMaybe (f dns x)
  where
  toMaybe (Left rc)
    | rc == sNXDOMAIN  = Just []
    | rc == sNODATA    = Just []
    | otherwise        = Nothing
  toMaybe (Right r)    = Just r

-- |Use this function to disable DNS resolving. It will
-- always return @('Answer' 'sSYSTEMFAIL' Nothing (Just
-- host) (-1) [])@.

dummyDNS :: Resolver
dummyDNS host _ _ = newMVar
  (Answer sSYSTEMFAIL Nothing (Just host) (-1) [])

-- |Print an IP host address as a string suitable for 'PTR' lookups.

toPTR :: HostAddress -> String
toPTR ha = shows b4 . ('.':) .
           shows b3 . ('.':) .
           shows b2 . ('.':) .
           shows b1 $ ".in-addr.arpa."
  where
    (b1,b2,b3,b4) = hostAddressToTuple ha

-- * Implementation

-- |The internal state of the resolver is stored in an
-- 'MVar' so that it is shared (and synchronized) between
-- any number of concurrent 'IO' threads.

data ResolverState = RState
  { adns     :: AdnsState               -- ^ opaque ADNS state
  , queries  :: Map Query (MVar Answer) -- ^ currently open queries
  }

-- |Submit a DNS query to the resolver and check whether we
-- have a running 'resolveLoop' thread already. If we don't,
-- start one with 'forkIO'. Make sure you link the threaded
-- RTS so that the main loop will not block other threads.

resolve :: MVar ResolverState -> Resolver
resolve mst r rt qfs = modifyMVar mst $ \st -> do
  res <- newEmptyMVar
  q <- adnsSubmit (adns st) r rt qfs
  when (Map.null (queries st))
    (forkIO (resolveLoop mst) >> return ())
  let st' = st { queries = Map.insert q res (queries st) }
  return (st', res)

-- |Loop until all open queries have been resolved.

resolveLoop :: MVar ResolverState -> IO ()
resolveLoop mst = do
  more <- modifyMVar mst $ \(RState dns qs) -> do
    r <- adnsWait dns
    case r of
      Nothing    -> return (RState dns qs, False)
      Just (q,a) -> do mv <- maybe (fail "inconsistent ADNS state") return (Map.lookup q qs)
                       putMVar mv a
                       return (RState dns (Map.delete q qs), True)
  when more (resolveLoop mst)
