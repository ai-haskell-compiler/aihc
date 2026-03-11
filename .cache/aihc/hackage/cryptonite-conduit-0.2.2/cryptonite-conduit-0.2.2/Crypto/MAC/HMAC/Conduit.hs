{-# LANGUAGE RankNTypes, BangPatterns #-}
-- |
-- Module      : Crypto.MAC.HMAC.Conduit
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing Conduit facilities for hmac based functions.
--
module Crypto.MAC.HMAC.Conduit
    ( -- * Cryptographic hash functions
      sinkHMAC
    ) where

import Crypto.Hash
import Crypto.MAC.HMAC
import Data.ByteArray
import Data.Conduit
import qualified Data.ByteString as BS

-- | A 'Sink' that calculates HMAC of a stream of 'B.ByteString'@s@ and
-- returns digest @d@.
sinkHMAC :: (Monad m, ByteArrayAccess key, HashAlgorithm hash) => key -> ConduitM BS.ByteString o m (HMAC hash)
sinkHMAC key = sink (initialize key)
  where sink ctx = do
            b <- await
            case b of
                Nothing -> return $! finalize ctx
                Just bs -> sink $! update ctx bs
