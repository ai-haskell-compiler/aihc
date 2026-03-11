module Network.Socket.Free (openFreePort, getFreePort) where
import qualified Network.Socket as N
import qualified Control.Exception as E
import qualified System.IO.Error as Error

-- | Open a TCP socket on a random free port. This is like 'warp''s
--   openFreePort.
--
--   Since 0.0.0.1
openFreePort :: IO (Int, N.Socket)
openFreePort =
  E.bracketOnError (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close
    $ \sock -> do
      N.bind sock $ N.SockAddrInet 0 $ N.tupleToHostAddress (127,0,0,1)
      N.getSocketName sock >>= \case
        N.SockAddrInet port _ -> pure (fromIntegral port, sock)
        addr -> E.throwIO
          $ Error.mkIOError Error.userErrorType
            (  "openFreePort was unable to create socket with a SockAddrInet. "
            <> "Got " <> show addr
            )
            Nothing
            Nothing

-- | Open a TCP socket, get its port and close the socket.
--   Useful when you have an external service that needs a fresh port.
--
--   There is a small race condition present:
--   It's possible to get a free port only for it to
--   be bound by some other process or thread before used
--
--   Since 0.2.1
getFreePort :: IO Int
getFreePort = do
  (port, socket) <- openFreePort
  N.close socket
  pure port
