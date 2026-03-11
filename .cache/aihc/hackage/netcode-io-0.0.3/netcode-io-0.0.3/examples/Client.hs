-- This example serves as a counterpart to the example program of the same name
-- in the C library.
module Main (main) where

import qualified Netcode.IO

import Control.Exception (try, throw, AsyncException(..))
import Control.Monad (forM_, when)
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess)

gProtocolID :: Word64
gProtocolID = 0x1122334455667788

gPrivateKey :: [Word8]
gPrivateKey =
    [ 0x60, 0x6a, 0xbe, 0x6e, 0xc9, 0x19, 0x10, 0xea
    , 0x9a, 0x65, 0x62, 0xf6, 0x6f, 0x2b, 0x30, 0xe4
    , 0x43, 0x71, 0xd6, 0x2c, 0xd1, 0x99, 0x27, 0x26
    , 0x6b, 0x3c, 0x60, 0xf4, 0xb7, 0x15, 0xab, 0xa1
    ]

gConnectTokenExpiry :: Int
gConnectTokenExpiry = 30

gConnectTokenTimeout :: Int
gConnectTokenTimeout = 5

main :: IO ()
main = do
    Netcode.IO.initialize

    Netcode.IO.logLevel Netcode.IO.LogLevel'Info

    putStrLn "[client]"
    let stateChangeCallback old new =
            putStrLn $ "Old state: " <> show old <> " new state: " <> show new
        clientConfig = Netcode.IO.setClientStateChangeCallback stateChangeCallback
                     $ Netcode.IO.defaultClientConfig
    client <- Netcode.IO.createClient "0.0.0.0" clientConfig 0.0

    args <- getArgs
    let serverAddr = case length args of
          1 -> head args
          _ -> "127.0.0.1:40000"

    -- This is expected to be performed on an HTTP server
    clientID <- Netcode.IO.generateClientID
    putStrLn $ "client id is " <> show clientID

    connectToken <- Netcode.IO.generateConnectToken [(serverAddr, serverAddr)]
                                                    gConnectTokenExpiry 
                                                    gConnectTokenTimeout
                                                    clientID
                                                    gProtocolID
                                                    gPrivateKey
                                                    []

    Netcode.IO.connectClient client connectToken

    let untilM :: IO Bool -> IO ()
        untilM cond = do
            tf <- cond
            if tf then return () else untilM cond

        catchUserInterrupt :: IO a -> IO a
        catchUserInterrupt prg = do
            interruptResult <- try prg
            case interruptResult of
                (Left UserInterrupt) -> putStrLn "\nshutting down" >> exitSuccess
                (Left e) -> throw e
                (Right x) -> return x

    quitResult <- try $ catchUserInterrupt $ forM_ [0.0, 0.016667 ..] $ \time -> do
        Netcode.IO.updateClient client time

        clientState <- Netcode.IO.getClientState client
        when (clientState == Netcode.IO.ClientState'Connected) $
            withArrayLen [0..(Netcode.IO.maximumPacketSize - 1)] $
            Netcode.IO.sendPacketFromClient client

        untilM $ do
            mpkt <- Netcode.IO.receivePacketFromServer client
            case mpkt of
                Nothing -> return True
                Just pkt -> do
                    withForeignPtr (Netcode.IO.packetDataPtr pkt) $ \pktMem -> do
                        pktData <- peekArray (Netcode.IO.packetSize pkt) pktMem
                        case (and $ zipWith (==) pktData [0,1..]) of
                            True -> return False
                            False -> fail "Received garbled packet!"
                    
        clientState' <- Netcode.IO.getClientState client
        when (any (== clientState') [ Netcode.IO.ClientState'ConnectTokenExpired
                                    , Netcode.IO.ClientState'InvalidConnectToken
                                    , Netcode.IO.ClientState'ConnectionTimedOut
                                    , Netcode.IO.ClientState'ConnectionResponseTimedOut
                                    , Netcode.IO.ClientState'ConnectionRequestTimedOut
                                    , Netcode.IO.ClientState'ConnectionDenied
                                    , Netcode.IO.ClientState'Disconnected
                                    ]) exitSuccess

        Netcode.IO.sleep 0.016667

    case quitResult of
        (Left ExitSuccess) -> return ()
        (Right _) -> fail "forM_ with infinite list terminated?"
        (Left e) -> fail $ show e

    Netcode.IO.destroyClient client
    Netcode.IO.terminate
