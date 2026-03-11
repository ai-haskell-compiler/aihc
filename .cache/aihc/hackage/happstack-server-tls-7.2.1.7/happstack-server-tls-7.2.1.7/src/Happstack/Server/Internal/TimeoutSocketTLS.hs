{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- |
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.Internal.TimeoutSocketTLS where

import           Control.Exception             (SomeException, catch)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import qualified Happstack.Server.Internal.TimeoutManager as TM
import           Happstack.Server.Internal.TimeoutIO (TimeoutIO(..))
import           Network.Socket                (Socket, close)
import           Network.Socket.SendFile (ByteCount, Offset)
import           OpenSSL.Session               (SSL)
import qualified OpenSSL.Session               as SSL
import           Prelude                       hiding (catch)
import           System.IO (IOMode(ReadMode), SeekMode(AbsoluteSeek), hSeek, withBinaryFile)
import           System.IO.Unsafe (unsafeInterleaveIO)

sPutLazyTickle :: TM.Handle -> SSL -> L.ByteString -> IO ()
sPutLazyTickle thandle ssl cs =
    do L.foldrChunks (\c rest -> SSL.write ssl c >> TM.tickle thandle >> rest) (return ()) cs
{-# INLINE sPutLazyTickle #-}

sPutTickle :: TM.Handle -> SSL -> B.ByteString -> IO ()
sPutTickle thandle ssl cs =
    do SSL.write ssl cs
       TM.tickle thandle
{-# INLINE sPutTickle #-}

sGet :: TM.Handle
     -> SSL
     -> IO (Maybe B.ByteString)
sGet handle ssl =
  do s <- SSL.read ssl chunkSize
     TM.tickle handle
     if S.null s
       then pure Nothing
       else pure (Just s)
  where
    chunkSize = 65536

sGetContents :: TM.Handle
             -> SSL              -- ^ Connected socket
             -> IO L.ByteString  -- ^ Data received
sGetContents handle ssl =
    fmap L.fromChunks loop
    where
      chunkSize = 65536
      loop = unsafeInterleaveIO $ do
               s <- SSL.read ssl chunkSize
               TM.tickle handle
               if S.null s
                then do return []
                else do ss <- loop
                        return (s:ss)

timeoutSocketIO :: TM.Handle -> Socket -> SSL -> TimeoutIO
timeoutSocketIO handle socket ssl =
    TimeoutIO { toHandle      = handle
              , toShutdown    = do SSL.shutdown ssl SSL.Unidirectional `catch` ignoreException
                                   close socket `catch` ignoreException
              , toPutLazy     = sPutLazyTickle handle ssl
              , toPut         = sPutTickle     handle ssl
              , toGet         = sGet           handle ssl
              , toGetContents = sGetContents   handle ssl
              , toSendFile    = sendFileTickle handle ssl
              , toSecure      = True
              }
    where
      ignoreException :: SomeException -> IO ()
      ignoreException _ = return ()

sendFileTickle :: TM.Handle -> SSL -> FilePath -> Offset -> ByteCount -> IO ()
sendFileTickle thandle ssl fp offset count =
    do withBinaryFile fp ReadMode $ \h -> do
         hSeek h AbsoluteSeek offset
         c <- L.hGetContents h
         sPutLazyTickle thandle ssl (L.take (fromIntegral count) c)
