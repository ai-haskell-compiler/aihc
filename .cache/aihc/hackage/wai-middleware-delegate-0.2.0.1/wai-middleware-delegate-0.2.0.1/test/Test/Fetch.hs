{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Fetch
  ( fetch
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
  ( ConduitT
  , SealedConduitT
  , Void
  , await
  , sealConduitT
  , yield
  , ($$+-)
  )
import qualified Data.Conduit.Binary as CB
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Network.Connection.CPP (noCheckSettings)
import Network.HTTP.Client
  ( BodyReader
  , Request
  , brRead
  , managerSetProxy
  , newManager
  , proxyFromRequest
  , responseBody
  , responseHeaders
  , responseStatus
  , secure
  , withResponse
  )
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (hContentLength, statusCode)
import Test.HttpReply


fetch :: Request -> IO HttpReply
fetch req = do
  let mSettings = mkManagerSettings noCheckSettings Nothing
      mSettings' = managerSetProxy proxyFromRequest mSettings
  m <- newManager mSettings'
  withResponse req m getSrc
  where
    getSrc resp = do
      let mbBodyLength = readInt64 <$> lookup hContentLength (responseHeaders resp)
      bodyText <- checkBodySize (sealConduitT $ bodyReaderSource $ responseBody resp) mbBodyLength
      return $ HttpReply (secure req) (statusCode $ responseStatus resp) (responseHeaders resp) bodyText


bodyCheckBlock :: Int64
bodyCheckBlock = 1000


checkBodySize :: (Monad m) => SealedConduitT () BS.ByteString m () -> Maybe Int64 -> m BS.ByteString
checkBodySize bodySrc Nothing = fmap (BS.concat . LBS.toChunks) $ bodySrc $$+- CB.take $ fromIntegral bodyCheckBlock
checkBodySize bodySrc (Just len)
  | len <= bodyCheckBlock = checkBodySize bodySrc Nothing
  | otherwise = fromMaybe "Success" <$> (bodySrc $$+- sizeCheckSink len)


-- A pipe that counts the size of each incoming C8.Bytestring, when the last is
-- received, the result is Nothing if the size matches the expected value or an
-- error message if it does not.
sizeCheckSink :: (Monad m) => Int64 -> ConduitT C8.ByteString Void m (Maybe C8.ByteString)
sizeCheckSink expectedSize = sink 0
  where
    sink !count = await >>= maybe (closeSink count) (sinkBlock count)
    sinkBlock !count bs = sink (count + fromIntegral (BS.length bs))

    -- \| no more bytes: return Nothing if the count so far matches the expected value
    closeSink !count
      | count == expectedSize = return Nothing
      | otherwise =
          return $
            Just . C8.pack $
              "Error : Body length "
                ++ show count
                ++ " should have been "
                ++ show expectedSize
                ++ "."


readInt64 :: C8.ByteString -> Int64
readInt64 = read . C8.unpack


bodyReaderSource ::
  (MonadIO m) =>
  BodyReader ->
  ConduitT i ByteString m ()
bodyReaderSource br =
  loop
  where
    loop = do
      bs <- liftIO $ brRead br
      unless (BS.null bs) $ do
        yield bs
        loop
