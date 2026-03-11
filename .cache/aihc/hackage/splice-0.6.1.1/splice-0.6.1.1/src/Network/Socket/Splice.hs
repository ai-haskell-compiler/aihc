{- |
  This library implements most efficient socket to socket data transfer loops
  for proxy servers on each operating system.
               
  On GNU\/Linux, it uses and exposes the zero-copy @splice()@ system call:
  <http://kerneltrap.org/node/6505>.

  On all other operating systems, it currently falls back to a portable Haskell
  implementation – which allocates a single memory buffer in user address space,
  then enters an inner loop that uses 'System.IO.hGetBufSome' and
  'System.IO.hPutBuf'. This avoids lots of tiny allocations as would otherwise
  be caused by 'Network.Socket.ByteString.recv' and
  'Network.Socket.ByteString.sendAll' from the @bytestring@ package.
-}
--  
-- Module      : Network.Socket.Splice
-- Copyright   : (c) Cetin Sert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : stable
-- Portability : works on all operating systems

module Network.Socket.Splice (

  module Network.Socket.Splice.Internal

  ) where

import Network.Socket.Splice.Internal
