{- |
   Module      :  ADNS
   Copyright   :  (c) 2008 Peter Simons
   License     :  LGPL

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   An asynchronous DNS resolver based on GNU ADNS
   <http://www.gnu.org/software/adns/>. You should link your
   program with the /threaded/ runtime-system when using this
   module. In GHC, this is accomplished by specifying @-threaded@
   on the command-line.
-}

module ADNS
  ( HostName, HostAddress
  , Resolver, initResolver, InitFlag(..)
  , queryA, queryPTR, queryMX, querySRV
  , dummyDNS
  )
  where

import Network.Socket    ( HostName, HostAddress, PortNumber )
import ADNS.Base
import ADNS.Resolver

queryA :: Resolver -> HostName -> IO (Maybe [HostAddress])
queryA = query resolveA

-- | For querying SRV records. Result is the list of tuples (host, port).
querySRV :: Resolver -> HostName -> IO (Maybe [(HostName, PortNumber)])
querySRV = query resolveSRV

queryPTR :: Resolver -> HostAddress -> IO (Maybe [HostName])
queryPTR = query resolvePTR

queryMX :: Resolver -> HostName -> IO (Maybe [(HostName, HostAddress)])
queryMX = query resolveMX
