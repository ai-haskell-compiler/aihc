{-- Originally from the lzma-conduit package
 -- https://hackage.haskell.org/package/lzma-conduit
 --
 -- Adapted & updated
 -}
module Data.Conduit.Lzma2(compress, compressWith, decompress, decompressWith) where

import qualified Codec.Compression.Lzma       as Lzma
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.Conduit
import           Data.Conduit.List            (peek)
import           Data.Maybe                   (fromMaybe)
import           Data.Word

prettyRet
  :: Lzma.LzmaRet
  -> String
prettyRet r = case r of
  Lzma.LzmaRetOK               -> "Operation completed successfully"
  Lzma.LzmaRetStreamEnd        -> "End of stream was reached"
  Lzma.LzmaRetUnsupportedCheck -> "Cannot calculate the integrity check"
  Lzma.LzmaRetGetCheck         -> "Integrity check type is now available"
  Lzma.LzmaRetMemError         -> "Cannot allocate memory"
  Lzma.LzmaRetMemlimitError    -> "Memory usage limit was reached"
  Lzma.LzmaRetFormatError      -> "File format not recognized"
  Lzma.LzmaRetOptionsError     -> "Invalid or unsupported options"
  Lzma.LzmaRetDataError        -> "Data is corrupt"
  Lzma.LzmaRetBufError         -> "No progress is possible"
  Lzma.LzmaRetProgError        -> "Programming error"


-- | Decompress a 'ByteString' from a lzma or xz container stream.
decompress
  :: (MonadThrow m, MonadIO m)
  => Maybe Word64 -- ^ Memory limit, in bytes.
  -> ConduitM ByteString ByteString m ()
decompress memlimit =
    decompressWith Lzma.defaultDecompressParams
                   { Lzma.decompressMemLimit     = fromMaybe maxBound memlimit
                   , Lzma.decompressAutoDecoder  = True
                   , Lzma.decompressConcatenated = True
                   }

decompressWith
  :: (MonadThrow m, MonadIO m)
  => Lzma.DecompressParams
  -> ConduitM ByteString ByteString m ()
decompressWith parms = do
    c <- peek
    case c of
      Nothing -> throwM $ userError "Data.Conduit.Lzma.decompress: invalid empty input"
      Just _  -> liftIO (Lzma.decompressIO parms) >>= go
  where
    go s@(Lzma.DecompressInputRequired more) = do
        mx <- await
        case mx of
          Just x
            | B.null x  -> go s -- ignore/skip empty bytestring chunks
            | otherwise -> liftIO (more x) >>= go
          Nothing       -> liftIO (more B.empty) >>= go
    go (Lzma.DecompressOutputAvailable output cont) = do
        yield output
        liftIO cont >>= go
    go (Lzma.DecompressStreamEnd rest)
        | B.null rest = return ()
        | otherwise   = leftover rest
    go (Lzma.DecompressStreamError err) =
        throwM $ userError $ "Data.Conduit.Lzma.decompress: error: "++prettyRet err


-- | Compress a 'ByteString' into a xz container stream.
compress
  :: (MonadIO m)
  => Maybe Int -- ^ Compression level from [0..9], defaults to 6.
  -> ConduitM ByteString ByteString m ()
compress level =
   compressWith Lzma.defaultCompressParams { Lzma.compressLevel = level' }
 where
   level' = case level of
              Nothing -> Lzma.CompressionLevel6
              Just n  -> let
                            n' = max minBound (min maxBound n) -- clamp to [minBound..maxBound] range
                        in toEnum n'

compressWith
  :: MonadIO m
  => Lzma.CompressParams
  -> ConduitM ByteString ByteString m ()
compressWith parms =
    liftIO (Lzma.compressIO parms) >>= go
  where
    go s@(Lzma.CompressInputRequired _flush more) = do
        mx <- await
        case mx of
          Just x
            | B.null x     -> go s -- ignore/skip empty bytestring chunks
            | otherwise    -> liftIO (more x) >>= go
          Nothing          -> liftIO (more B.empty) >>= go
    go (Lzma.CompressOutputAvailable output cont) = do
        yield output
        liftIO cont >>= go
    go Lzma.CompressStreamEnd = return ()
