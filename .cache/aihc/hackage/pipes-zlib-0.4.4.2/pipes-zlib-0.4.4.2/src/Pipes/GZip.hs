{-# LANGUAGE RankNTypes #-}

-- | This module exports utilities to compress and decompress GZip @pipes@
-- streams.

module Pipes.GZip (
  -- * Streams
    decompress
  , decompress'
  , compress

  -- * Compression level
  , Pipes.Zlib.CompressionLevel
  , Pipes.Zlib.defaultCompression
  , Pipes.Zlib.noCompression
  , Pipes.Zlib.bestSpeed
  , Pipes.Zlib.bestCompression
  , Pipes.Zlib.compressionLevel
  ) where

import qualified Data.Streaming.Zlib       as Zlib
import qualified Data.ByteString           as B
import           Pipes
import qualified Pipes.Zlib

--------------------------------------------------------------------------------

-- | Decompress bytes flowing from a 'Producer'.
--
-- @
-- 'decompress' :: 'MonadIO' m
--            => 'Producer' 'B.ByteString' m r
--            -> 'Producer' 'B.ByteString' m r
-- @
decompress
  :: MonadIO m
  => Proxy x' x () B.ByteString m r -- ^ Compressed stream
  -> Proxy x' x () B.ByteString m r -- ^ Decompressed stream
decompress = Pipes.Zlib.decompress gzWindowBits
{-# INLINABLE decompress #-}

-- | Decompress bytes flowing from a 'Producer', returning any leftover input
-- that follows the compressed stream.
decompress'
  :: MonadIO m
  => Producer B.ByteString m r -- ^ Compressed stream
  -> Producer B.ByteString m (Either (Producer B.ByteString m r) r)
     -- ^ Decompressed stream, returning either a 'Producer' of the leftover input
     -- or the return value from the input 'Producer'.
decompress' = Pipes.Zlib.decompress' gzWindowBits
{-# INLINABLE decompress' #-}


-- | Compress bytes flowing from a 'Producer'.
--
-- @
-- 'compress' :: 'MonadIO' m
--          => 'ZC.CompressionLevel'
--          -> 'Producer' 'B.ByteString' m r
--          -> 'Producer' 'B.ByteString' m r
-- @
compress
  :: MonadIO m
  => Pipes.Zlib.CompressionLevel
  -> Proxy x' x () B.ByteString m r -- ^ Decompressed stream
  -> Proxy x' x () B.ByteString m r -- ^ Compressed stream
compress clevel = Pipes.Zlib.compress clevel gzWindowBits
{-# INLINABLE compress #-}

gzWindowBits :: Zlib.WindowBits
gzWindowBits = Zlib.WindowBits 31
