{-# LANGUAGE RankNTypes, BangPatterns #-}
-- |
-- Module      : Crypto.Hash.Conduit
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing Conduit facilities for hash based functions.
--
-- this module is vaguely similar to the crypto-conduit part related to hash
-- on purpose, as to provide an upgrade path. The api documentation is pulled
-- directly from this package and adapted, and thus are originally
-- copyright Felipe Lessa.
--
module Crypto.Hash.Conduit
    ( -- * Cryptographic hash functions
      sinkHash
    , hashFile
    ) where

import Crypto.Hash
import qualified Data.ByteString as B

import Data.Conduit
import Data.Conduit.Binary (sourceFile)

import Control.Monad.IO.Class (MonadIO, liftIO)

-- | A 'Sink' that hashes a stream of 'B.ByteString'@s@ and
-- creates a digest @d@.
sinkHash :: (Monad m, HashAlgorithm hash) => ConduitT B.ByteString o m (Digest hash)
sinkHash = sink hashInit
  where sink ctx = do
            b <- await
            case b of
                Nothing -> return $! hashFinalize ctx
                Just bs -> sink $! hashUpdate ctx bs

-- | Hashes the whole contents of the given file in constant
-- memory.  This function is just a convenient wrapper around
-- 'sinkHash' defined as:
--
-- @
-- hashFile fp = 'liftIO' $ 'runConduitRes' ('sourceFile' fp '.|' 'sinkHash')
-- @
hashFile :: (MonadIO m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashFile fp = liftIO $ runConduitRes (sourceFile fp .| sinkHash)
