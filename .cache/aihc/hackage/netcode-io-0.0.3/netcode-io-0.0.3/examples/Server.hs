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

main :: IO ()
main = do
    Netcode.IO.initialize

    Netcode.IO.logLevel Netcode.IO.LogLevel'Info

    putStrLn "[server]"
    args <- getArgs
    let serverAddr = if length args == 1 then head args else "127.0.0.1:40000"
        serverConfig =
            Netcode.IO.setPrivateKey gPrivateKey $
            Netcode.IO.setProtocolID gProtocolID $
            Netcode.IO.defaultServerConfig
    server <- Netcode.IO.createServer serverAddr serverConfig 0.0

    Netcode.IO.startServer server Netcode.IO.maxNumClients

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
        Netcode.IO.updateServer server time

        clientConnected <- Netcode.IO.clientConnectedAtIndex server 0
        when clientConnected $
            withArrayLen [0..(Netcode.IO.maximumPacketSize - 1)] $
            Netcode.IO.sendPacketFromServer server 0

        forM_ [0..(Netcode.IO.maxNumClients - 1)] $ \client -> untilM $ do
          mpkt <- Netcode.IO.receivePacketFromClient server client
          case mpkt of
            Nothing -> return True
            Just pkt -> withForeignPtr (Netcode.IO.packetDataPtr pkt) $ \pktMem -> do
                pktData <- peekArray (Netcode.IO.packetSize pkt) pktMem
                case (and $ zipWith (==) pktData [0,1..]) of
                  True -> return False
                  False -> fail "Received garbled packet!"

        Netcode.IO.sleep 0.016667

    case quitResult of
        (Left ExitSuccess) -> return ()
        (Right _) -> fail "forM_ with infinite list terminated?"
        (Left e) -> fail $ show e

    Netcode.IO.destroyServer server
    Netcode.IO.terminate
