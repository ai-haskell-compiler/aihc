import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Test.Hspec
import Network.FTP.Client hiding (Success)
import qualified Network.FTP.Client as F
import Control.Monad.IO.Class
import Control.Concurrent.MVar

data TestHandleMVars = TestHandleMVars
    { thmSend :: MVar [ByteString]
    , thmSendLine :: MVar [ByteString]
    , thmRecv :: MVar [Int]
    }

data TestHandle = TestHandle TestHandleMVars Handle

testHandle
    :: [ByteString]
    -> [ByteString]
    -> Security
    -> IO TestHandle
testHandle recvResps recvLineResps sec = do
    sendMVar <- newMVar []
    sendLineMVar <- newMVar []
    recvMVar <- newMVar []
    recvCount <- newMVar 0
    recvLineCount <- newMVar 0
    let testHandleMVars = TestHandleMVars
            sendMVar sendLineMVar recvMVar
        handle = Handle
            { send = \s ->
                modifyMVar_ sendMVar
                    (\ss -> return $ ss <> [s])
            , sendLine = \s ->
                modifyMVar_ sendLineMVar
                    (\ss -> return $ ss <> [s])
            , recv = \i -> do
                modifyMVar_ recvMVar
                    (\is -> return $ is <> [i])
                (recvResps !!) <$> modifyMVar recvCount
                    (\i -> return (i + 1, i))
            , recvLine =
                (recvLineResps !!) <$> modifyMVar recvLineCount
                    (\i -> return (i + 1, i))
            , security = sec
            }
    return $ TestHandle testHandleMVars handle

main :: IO ()
main = hspec $ do
    describe "Network.FTP.Client.sendCommand" $ do
        it "sends USER for User" $ do
            let expected = FTPResponse
                    F.Success 200
                    (SingleLine $ C.pack "Ok")
            (TestHandle mvars h) <- testHandle [] [C.pack "200 Ok"] Clear
            sendCommand h (User "megan") `shouldReturn` expected
            takeMVar (thmSend mvars) `shouldReturn` [C.pack "USER megan\r\n"]
        it "sends USER for User and receives a multiline response" $ do
            let expected = FTPResponse
                    F.Success 200
                    (MultiLine [C.pack "line1", C.pack "line2", C.pack "200 line3"])
            (TestHandle mvars h) <- testHandle []
                [ C.pack "200-line1\r\n"
                , C.pack "line2\r\n"
                , C.pack "200 line3\r\n"
                ] Clear
            sendCommand h (User "megan") `shouldReturn` expected
            takeMVar (thmSend mvars) `shouldReturn` [C.pack "USER megan\r\n"]
    describe "Network.FTP.Client.recvAll" $
        it "doesn't hang on empty response" $ do
            let expected = C.pack ""
            (TestHandle mvars h) <- testHandle [C.pack ""] [] Clear
            recvAll h `shouldReturn` expected
