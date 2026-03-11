-- | Module for sending segments to the XRay daemon.

module Network.AWS.XRayClient.SendSegments
  ( sendSegmentsToDaemon
  , NoAddressInfoException(..)
  ) where

import Prelude

import Control.Exception (Exception, SomeException, catch, throwIO)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Network.AWS.XRayClient.Segment as X
import Network.Socket
import Network.Socket.ByteString (sendAll)
import System.IO (hPutStrLn, stderr)

-- | Makes JSON payloads for each segment and sends it to the daemon.
sendSegmentsToDaemon :: Text -> Int -> [XRaySegment] -> IO ()
sendSegmentsToDaemon host port segments = catch
  (sendUDPByteStrings host port $ makeXRayPayload <$> segments)
  (\(err :: SomeException) ->
    hPutStrLn stderr ("sendUDPByteStrings: " ++ show err)
  )

makeXRayPayload :: XRaySegment -> ByteString
makeXRayPayload segment =
  let header = object ["format" .= ("json" :: String), "version" .= (1 :: Int)]
  in BSL.toStrict $ encode header <> "\n" <> encode segment

-- | Sends the entirety of multiple 'ByteString' values to a given host/port
-- via UDP Datagrams.
sendUDPByteStrings :: Text -> Int -> [ByteString] -> IO ()
sendUDPByteStrings host port payloads = do
  addrInfos <- getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
  case addrInfos of
    [] ->
      throwIO
        $ NoAddressInfoException
        $ "No address info for "
        ++ T.unpack host
        ++ ":"
        ++ show port
    (serverAddr : _) -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      connect sock (addrAddress serverAddr)
      traverse_ (sendAll sock) payloads
      close sock

newtype NoAddressInfoException
  = NoAddressInfoException String
  deriving (Show, Typeable)

instance Exception NoAddressInfoException
