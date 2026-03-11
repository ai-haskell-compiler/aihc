module Data.Conduit.Zstd
    ( compress
    , decompress
    ) where

import qualified Data.Conduit as C
import qualified Data.ByteString as B
import qualified Codec.Compression.Zstd.Streaming as Z
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception.Base (throwIO)
import           System.IO.Error (userError)
import           Data.Maybe (fromMaybe)


-- | compression conduit
compress :: MonadIO m =>
        Int -- ^ compression level
        -> C.ConduitT B.ByteString B.ByteString m ()
compress level = liftIO (Z.compress level) >>= go

-- | decompression conduit
decompress :: MonadIO m => C.ConduitT B.ByteString B.ByteString m ()
decompress = liftIO Z.decompress >>= go

go :: MonadIO m => Z.Result -> C.ConduitT B.ByteString B.ByteString m ()
go (Z.Produce r next) = do
    C.yield r
    liftIO next >>= go
go input@(Z.Consume f) = do
    next <- C.await
    case next of
      Just chunk | B.null chunk ->
        go input
      _ ->
        liftIO (f $ fromMaybe B.empty next) >>= go
go (Z.Error m e) = liftIO (throwIO $ userError ("ZStandard error :" ++ m ++ ": " ++ e))
go (Z.Done r) = C.yield r
