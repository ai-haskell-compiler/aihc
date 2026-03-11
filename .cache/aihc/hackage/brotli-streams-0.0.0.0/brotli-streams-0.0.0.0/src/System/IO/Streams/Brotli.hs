{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : System.IO.Streams.Brotli
-- Copyright   : © 2016 Herbert Valerio Riedel
-- License     : BSD3
--
-- Maintainer  : hvr@gnu.org
--
-- Simple IO-Streams interface for Brotli compression (<https://tools.ietf.org/html/rfc7932 RFC7932>).
--
module System.IO.Streams.Brotli
    ( -- * Simple interface
      compress
    , decompress

      -- * Extended interface
      -- ** Compression
    , compressWith

    , Brotli.defaultCompressParams
    , Brotli.CompressParams
    , Brotli.compressLevel
    , Brotli.compressWindowSize
    , Brotli.compressMode
    , Brotli.compressSizeHint
    , Brotli.CompressionLevel(..)
    , Brotli.CompressionWindowSize(..)
    , Brotli.CompressionMode(..)

      -- ** Decompression
    , decompressWith

    , Brotli.defaultDecompressParams
    , Brotli.DecompressParams
    , Brotli.decompressDisableRingBufferReallocation

    ) where

import           Codec.Compression.Brotli (DecompressStream(..), CompressStream(..))
import qualified Codec.Compression.Brotli as Brotli

import           Control.Exception
import           Control.Monad
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.IORef
import           Data.Maybe
import           System.IO.Streams      (InputStream, OutputStream,
                                         makeInputStream, makeOutputStream)
import qualified System.IO.Streams      as Streams

-- | Decompress a Brotli-compressed 'InputStream' of strict 'ByteString's
decompress :: InputStream ByteString -> IO (InputStream ByteString)
decompress = decompressWith Brotli.defaultDecompressParams

-- | Like 'decompress' but with the ability to specify various decompression
-- parameters. Typical usage:
--
-- > decompressWith defaultDecompressParams { decompress... = ... }
decompressWith :: Brotli.DecompressParams -> InputStream ByteString -> IO (InputStream ByteString)
decompressWith parms ibs = do
    st <- newIORef =<< Brotli.decompressIO parms
    makeInputStream (go st)
  where
    go stref = do
        st' <- goFeed =<< readIORef stref
        case st' of
            DecompressInputRequired _ -> do
                writeIORef stref st'
                fail "the impossible happened"

            DecompressOutputAvailable outb next -> do
                writeIORef stref =<< next
                return (Just outb)

            DecompressStreamEnd leftover -> do
                unless (BS.null leftover) $ do
                    Streams.unRead leftover ibs
                writeIORef stref (DecompressStreamEnd BS.empty)
                return Nothing

            DecompressStreamError rc -> do
                writeIORef stref st'
                throwIO rc

    -- feed engine
    goFeed (DecompressInputRequired supply) =
        goFeed =<< supply . fromMaybe BS.empty =<< getChunk
    goFeed s = return s

    -- wrapper around 'read ibs' to retry until a non-empty ByteString or Nothing is returned
    getChunk = do
        mbs <- Streams.read ibs
        case mbs of
            Just bs | BS.null bs -> getChunk
            _                    -> return mbs

----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- | Convert an 'OutputStream' that consumes compressed 'ByteString's
-- (in the Brotli format) into an 'OutputStream' that consumes
-- uncompressed 'ByteString's
compress :: OutputStream ByteString -> IO (OutputStream ByteString)
compress = compressWith Brotli.defaultCompressParams

-- | Like 'compress' but with the ability to specify various compression
-- parameters. Typical usage:
--
-- > compressWith defaultCompressParams { compress... = ... }
compressWith :: Brotli.CompressParams -> OutputStream ByteString -> IO (OutputStream ByteString)
compressWith parms obs = do
    st <- newIORef =<< Brotli.compressIO parms
    makeOutputStream (go st)
  where
    go stref (Just chunk) = do
        st <- readIORef stref
        st' <- case st of
            CompressInputRequired flush supply
              | BS.null chunk -> goOutput True =<< flush
              | otherwise     -> goOutput False =<< supply chunk
            _ -> fail "compressWith: unexpected state"
        writeIORef stref st'
        case st' of
            CompressInputRequired _ _ -> return ()
            _ -> fail "compressWith:  unexpected state"

    -- EOF
    go stref Nothing = do
        st <- readIORef stref
        st' <- case st of
            CompressInputRequired _ supply -> goOutput False =<< supply BS.empty
            _ -> fail "compressWith[EOF]: unexpected state"
        writeIORef stref st'
        case st' of
            CompressStreamEnd -> return ()
            _ -> fail "compressWith[EOF]:  unexpected state"

    -- Drain output from CompressStream
    goOutput flush st@(CompressInputRequired _ _) = do
        when flush $
            Streams.write (Just BS.empty) obs
        return st
    goOutput flush (CompressOutputAvailable obuf next) = do
        Streams.write (Just obuf) obs
        goOutput flush =<< next
    goOutput _ st@CompressStreamEnd = do
        Streams.write Nothing obs
        return st
