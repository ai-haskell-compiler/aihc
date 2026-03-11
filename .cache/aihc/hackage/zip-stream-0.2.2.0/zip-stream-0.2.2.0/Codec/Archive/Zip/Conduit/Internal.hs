{-# LANGUAGE BangPatterns #-}
module Codec.Archive.Zip.Conduit.Internal
  ( osVersion, zipVersion
  , zipError
  , idConduit
  , sizeCRC
  , outputSize
  , inputSize
  , maxBound32
  , deflateWindowBits
  ) where

import           Codec.Compression.Zlib.Raw (WindowBits(..))
import           Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import           Data.Digest.CRC32 (crc32Update)
import           Data.Word (Word8, Word32, Word64)

import           Codec.Archive.Zip.Conduit.Types

-- | The version of this zip program, really just rough indicator of compatibility
zipVersion :: Word8
zipVersion = 48

-- | The OS this implementation tries to be compatible to
osVersion :: Word8
osVersion = 0 -- DOS

zipError :: MonadThrow m => String -> m a
zipError = throwM . ZipError

idConduit :: Monad m => C.ConduitT a a m ()
idConduit = C.awaitForever C.yield

passthroughFold :: Monad m => (a -> b -> a) -> a -> C.ConduitT b b m a
passthroughFold f !z = C.await >>= maybe
  (return z)
  (\x -> do
    C.yield x
    passthroughFold f (f z x))

sizeCRC :: Monad m => C.ConduitT BS.ByteString BS.ByteString m (Word64, Word32)
sizeCRC = passthroughFold (\(!l, !c) b -> (l + fromIntegral (BS.length b), crc32Update c b)) (0, 0)

sizeC :: Monad m => C.ConduitT BS.ByteString BS.ByteString m Word64
sizeC = passthroughFold (\l b -> l + fromIntegral (BS.length b)) 0 -- fst <$> sizeCRC

outputSize :: Monad m => C.ConduitT i BS.ByteString m () -> C.ConduitT i BS.ByteString m Word64
outputSize = (C..| sizeC)

inputSize :: Monad m => C.ConduitT BS.ByteString o m () -> C.ConduitT BS.ByteString o m Word64
-- inputSize = fuseUpstream sizeC -- won't work because we need to deal with leftovers properly
inputSize (CI.ConduitT src) = CI.ConduitT $ \rest -> let
  go n (CI.Done ()) = rest n
  go n (CI.PipeM m) = CI.PipeM $ go n <$> m
  go n (CI.Leftover p b) = CI.Leftover (go (n - fromIntegral (BS.length b)) p) b
  go n (CI.HaveOutput p o) = CI.HaveOutput (go n p) o
  go n (CI.NeedInput p q) = CI.NeedInput (\b -> go (n + fromIntegral (BS.length b)) (p b)) (go n . q)
  in go 0 (src CI.Done)

maxBound32 :: Integral n => n
maxBound32 = fromIntegral (maxBound :: Word32)

deflateWindowBits :: WindowBits
deflateWindowBits = WindowBits (-15)
