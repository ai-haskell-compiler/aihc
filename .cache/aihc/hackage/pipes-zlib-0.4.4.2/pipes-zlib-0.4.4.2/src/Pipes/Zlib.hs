{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module exports utilities to compress and decompress @pipes@ streams
-- using the zlib compression codec.
--
-- If you want to compress or decompress GZip streams, use the "Pipes.GZip"
-- module instead.

module Pipes.Zlib (
  -- * Streams
    decompress
  , decompress'
  , compress

  -- * Compression levels
  , CompressionLevel
  , defaultCompression
  , noCompression
  , bestSpeed
  , bestCompression
  , compressionLevel

  -- * Window size
  -- $ccz-re-export
  , Z.defaultWindowBits
  , windowBits
  ) where

import           Data.Streaming.Zlib       as Z
import           Control.Exception         (throwIO)
import           Control.Monad             (unless)
import qualified Data.ByteString           as B
import           Pipes

--------------------------------------------------------------------------------

-- | Decompress bytes flowing from a 'Producer'.
--
-- See the "Codec.Compression.Zlib" module for details about 'Z.WindowBits'.
--
-- @
-- 'decompress' :: 'MonadIO' m
--            => 'Z.WindowBits'
--            => 'Producer' 'B.ByteString' m r
--            -> 'Producer' 'B.ByteString' m r
-- @
decompress
  :: MonadIO m
  => Z.WindowBits
  -> Proxy x' x () B.ByteString m r -- ^ Compressed stream
  -> Proxy x' x () B.ByteString m r -- ^ Decompressed stream
decompress wbits p0 = do
    inf <- liftIO $ Z.initInflate wbits
    r <- for p0 $ \bs -> do
       popper <- liftIO (Z.feedInflate inf bs)
       fromPopper popper
    bs <- liftIO $ Z.finishInflate inf
    unless (B.null bs) (yield bs)
    return r
{-# INLINABLE decompress #-}

-- | Decompress bytes flowing from a 'Producer', returning any leftover input
-- that follows the compressed stream.
decompress'
  :: MonadIO m
  => Z.WindowBits
  -> Producer B.ByteString m r -- ^ Compressed stream
  -> Producer B.ByteString m (Either (Producer B.ByteString m r) r)
     -- ^ Decompressed stream, ending with either leftovers or a result
decompress' wbits p0 = go p0 =<< liftIO (Z.initInflate wbits)
  where
    flush inf = do
      bs <- liftIO $ Z.flushInflate inf
      unless (B.null bs) (yield bs)
    go p inf = do
      res <- lift (next p)
      case res of
         Left r -> return $ Right r
         Right (bs, p') -> do
            fromPopper =<< liftIO (Z.feedInflate inf bs)
            flush inf
            leftover <- liftIO $ Z.getUnusedInflate inf
            if B.null leftover
               then go p' inf
               else return $ Left (yield leftover >> p')
{-# INLINABLE decompress' #-}

-- | Compress bytes flowing from a 'Producer'.
--
-- See the "Codec.Compression.Zlib" module for details about
-- 'Z.CompressionLevel' and 'Z.WindowBits'.
--
-- @
-- 'compress' :: 'MonadIO' m
--          => 'Z.CompressionLevel'
--          -> 'Z.WindowBits'
--          -> 'Producer' 'B.ByteString' m r
--          -> 'Producer' 'B.ByteString' m r
-- @
compress
  :: MonadIO m
  => CompressionLevel
  -> Z.WindowBits
  -> Proxy x' x () B.ByteString m r -- ^ Decompressed stream
  -> Proxy x' x () B.ByteString m r -- ^ Compressed stream
compress (CompressionLevel clevel) wbits p0 = do
    def <- liftIO $ Z.initDeflate clevel wbits
    r <- for p0 $ \bs -> do
       popper <- liftIO (Z.feedDeflate def bs)
       fromPopper popper
    fromPopper $ Z.finishDeflate def
    return r
{-# INLINABLE compress #-}

--------------------------------------------------------------------------------

-- $ccz-re-export
--
-- The following are re-exported from "Codec.Compression.Zlib" for your
-- convenience.

--------------------------------------------------------------------------------
-- Compression Levels

-- | How hard should we try to compress?
newtype CompressionLevel = CompressionLevel Int
                         deriving (Show, Read, Eq, Ord)

defaultCompression, noCompression, bestSpeed, bestCompression :: CompressionLevel
defaultCompression = CompressionLevel (-1)
noCompression      = CompressionLevel 0
bestSpeed          = CompressionLevel 1
bestCompression    = CompressionLevel 9

-- | A specific compression level between 0 and 9.
compressionLevel :: Int -> CompressionLevel
compressionLevel n
  | n >= 0 && n <= 9 = CompressionLevel n
  | otherwise        = error "CompressionLevel must be in the range 0..9"

windowBits :: Int -> WindowBits
windowBits = WindowBits

--------------------------------------------------------------------------------
-- Internal stuff

-- | Produce values from the given 'Z.Popper' until exhausted.
fromPopper :: MonadIO m
           => Z.Popper
           -> Producer' B.ByteString m ()
fromPopper pop = loop
  where
    loop = do
      mbs <- liftIO pop
      case mbs of
          PRDone     -> return ()
          PRError e  -> liftIO $ throwIO e
          PRNext bs  -> yield bs >> loop
{-# INLINABLE fromPopper #-}
