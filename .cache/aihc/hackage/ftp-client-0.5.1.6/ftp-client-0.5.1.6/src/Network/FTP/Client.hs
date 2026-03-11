{-|
Module      : Network.FTP.Client
Description : Transfer files over FTP and FTPS
License     : Public Domain
Stability   : experimental
Portability : POSIX
-}
module Network.FTP.Client (
    -- * Main Entrypoints
    withFTP,
    withFTPS,
    -- * Control Commands
    login,
    pasv,
    rename,
    dele,
    cwd,
    size,
    mkd,
    rmd,
    pwd,
    quit,
    -- * Data Commands
    nlst,
    retr,
    list,
    stor,
    mlsd,
    mlst,
    -- * Types
    FTPCommand(..),
    FTPResponse(..),
    FTPMessage(..),
    ResponseStatus(..),
    MlsxResponse(..),
    RTypeCode(..),
    PortActivity(..),
    ProtType(..),
    Security(..),
    Handle(..),
    -- * Exceptions
    FTPException(..),
    -- * Handle Implementations
    sIOHandleImpl,
    tlsHandleImpl,
    -- * Lower Level Functions
    sendCommand,
    sendCommandS,
    recvAll,
    sendAll,
    sendAllS,
    getLineResp,
    getResponse,
    getResponseS,
    sendCommandLine,
    createSendDataCommand,
    createTLSSendDataCommand,
    parseMlsxLine
) where

import Data.Default.Class (def)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.List
import Data.Attoparsec.ByteString.Char8
import qualified Network.Socket as S
import qualified System.IO as SIO
import Data.Monoid ((<>))
import Control.Exception
import Control.Monad.Catch (MonadCatch, MonadMask)
import qualified Control.Monad.Catch as M
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Network.Connection
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Arrow
import Data.Typeable

debugging :: Bool
debugging = False

debugPrint :: (Show a, MonadIO m) => a -> m ()
debugPrint s = when debugging (liftIO $ print s)

debugResponse :: (Show a, MonadIO m) => a -> m ()
debugResponse s = debugPrint $ "Recieved: " <> show s

data Security = Clear | TLS

-- | Can send and recieve a 'Data.ByteString.ByteString'.
data Handle = Handle
    { send :: ByteString -> IO ()
    , sendLine :: ByteString -> IO ()
    , recv :: Int -> IO ByteString
    , recvLine :: IO ByteString
    , security :: Security
    }

data FTPMessage = SingleLine ByteString | MultiLine [ByteString]
    deriving Eq

instance Show FTPMessage where
    show (SingleLine message) = C.unpack message
    show (MultiLine messages) = intercalate "\n" $ C.unpack <$> messages

-- | Response from an FTP command. ex "200 Welcome!"
data FTPResponse = FTPResponse {
    frStatus :: ResponseStatus, -- ^ Interpretation of the first digit of an FTP response code
    frCode :: Int, -- ^ The three digit response code
    frMessage :: FTPMessage -- ^ Text of the response
} deriving Eq

instance Show FTPResponse where
    show fr = show (frCode fr) <> " " <> show (frMessage fr)

-- | First digit of an FTP response
data ResponseStatus
    = Wait -- ^ 1
    | Success -- ^ 2
    | Continue -- ^ 3
    | FailureRetry -- ^ 4
    | Failure -- ^ 5
    deriving (Show, Eq)

data FTPException
    = FailureRetryException FTPResponse
    | FailureException FTPResponse
    | UnsuccessfulException FTPResponse
    | BogusResponseFormatException FTPResponse
    | BadProtocolResponseException ByteString
    deriving (Show, Typeable)

instance Exception FTPException

responseStatus :: ByteString -> ResponseStatus
responseStatus cbs =
    case C.uncons cbs of
        Just ('1', _) -> Wait
        Just ('2', _) -> Success
        Just ('3', _) -> Continue
        Just ('4', _) -> FailureRetry
        Just ('5', _) -> Failure
        _ -> throw $ BadProtocolResponseException cbs

data RTypeCode = TA | TI

serialzeRTypeCode :: RTypeCode -> String
serialzeRTypeCode TA = "A"
serialzeRTypeCode TI = "I"

data PortActivity = Active | Passive

data ProtType = P | C

-- | Commands according to the FTP specification
data FTPCommand
    = User String
    | Pass String
    | Acct String
    | RType RTypeCode
    | Retr String
    | Nlst [String]
    | Port S.HostAddress S.PortNumber
    | Stor String
    | List [String]
    | Rnfr String
    | Rnto String
    | Dele String
    | Size String
    | Mkd String
    | Rmd String
    | Pbsz Int
    | Prot ProtType
    | Mlsd String
    | Mlst String
    | Cwd String
    | Cdup
    | Ccc
    | Auth
    | Pwd
    | Abor
    | Pasv
    | Quit

instance Show FTPCommand where
    show = serializeCommand

formatPort :: S.HostAddress -> S.PortNumber -> String
formatPort ha pn =
    let (w1, w2, w3, w4) = S.hostAddressToTuple ha
        hn = show <$> [w1, w2, w3, w4]
        portParts = show <$> [pn `quot` 256, pn `mod` 256]
    in intercalate "," (hn <> portParts)

serializeCommand :: FTPCommand -> String
serializeCommand (User user)  = "USER " <> user
serializeCommand (Pass pass)  = "PASS " <> pass
serializeCommand (Acct acct)  = "ACCT " <> acct
serializeCommand (RType rt)   = "TYPE " <> serialzeRTypeCode rt
serializeCommand (Retr file)  = "RETR " <> file
serializeCommand (Nlst [])    = "NLST"
serializeCommand (Nlst args)  = "NLST " <> unwords args
serializeCommand (Port ha pn) = "PORT " <> formatPort ha pn
serializeCommand (Stor loc)   = "STOR " <> loc
serializeCommand (List [])    = "LIST"
serializeCommand (List args)  = "LIST " <> unwords args
serializeCommand (Rnfr from)  = "RNFR " <> from
serializeCommand (Rnto to)    = "RNTO " <> to
serializeCommand (Dele file)  = "DELE " <> file
serializeCommand (Size file)  = "SIZE " <> file
serializeCommand (Mkd dir)    = "MKD " <> dir
serializeCommand (Rmd dir)    = "RMD " <> dir
serializeCommand (Pbsz buf)   = "PBSZ " <> show buf
serializeCommand (Prot P)     = "PROT P"
serializeCommand (Prot C)     = "PROT C"
serializeCommand (Mlsd path)  = "MLSD " <> path
serializeCommand (Mlst path)  = "MLST " <> path
serializeCommand (Cwd dir)    = "CWD " <> dir
serializeCommand Cdup         = "CDUP"
serializeCommand Ccc          = "CCC"
serializeCommand Auth         = "AUTH TLS"
serializeCommand Pwd          = "PWD"
serializeCommand Abor         = "ABOR"
serializeCommand Pasv         = "PASV"
serializeCommand Quit         = "QUIT"

stripCLRF :: ByteString -> ByteString
stripCLRF = C.takeWhile $ (&&) <$> (/= '\r') <*> (/= '\n')

-- | Get a line from the server
getLineResp :: Handle -> IO ByteString
getLineResp h = stripCLRF <$> recvLine h

-- | Get a full response from the server
-- Used in 'sendCommand'
getResponse :: MonadIO m => Handle -> m FTPResponse
getResponse h = do
    line <- liftIO $ getLineResp h
    let (code, rest) = C.splitAt 3 line
    message <- if C.head rest == '-'
        then MultiLine <$> loopMultiLine h code [line]
        else return $ SingleLine line
    let codeDroppedMessage = case message of
            SingleLine message -> SingleLine $ C.drop 4 message
            MultiLine [] -> MultiLine []
            MultiLine (message:messages) ->
                MultiLine $ C.drop 4 message : messages
    let response = FTPResponse
            (responseStatus code)
            (read $ C.unpack code)
            codeDroppedMessage
    case frStatus response of
        FailureRetry -> liftIO $ throwIO $ FailureRetryException response
        Failure -> liftIO $ throwIO $ FailureException response
        _ -> return response

loopMultiLine
    :: MonadIO m
    => Handle
    -> ByteString
    -> [ByteString]
    -> m [ByteString]
loopMultiLine h code lines = do
    nextLine <- liftIO $ getLineResp h
    let newLines = lines <> [C.dropWhile (== ' ') nextLine]
        nextCode = C.take 3 nextLine
    if nextCode == code
        then return newLines
        else loopMultiLine h code newLines

ensureSuccess :: MonadIO m => FTPResponse -> m FTPResponse
ensureSuccess resp =
    case frStatus resp of
        Success -> return resp
        _ -> liftIO $ throwIO $ UnsuccessfulException resp

getResponseS :: MonadIO m => Handle -> m FTPResponse
getResponseS = ensureSuccess <=< getResponse

sendCommandLine :: MonadIO m => Handle -> ByteString -> m ()
sendCommandLine h = liftIO . send h . (<> "\r\n")

-- | Send a command to the server and get a response back.
-- Some commands use a data 'Handle', and their data is not returned here.
sendCommand :: MonadIO m => Handle -> FTPCommand -> m FTPResponse
sendCommand h fc = do
    let command = serializeCommand fc
    debugPrint $ "Sending: " <> command
    sendCommandLine h $ C.pack command
    resp <- getResponse h
    debugResponse resp
    return resp

sendCommandS :: MonadIO m => Handle -> FTPCommand -> m FTPResponse
sendCommandS h fc = sendCommand h fc >>= ensureSuccess

-- | Equvalent to
--
-- > mapM . sendCommand
sendAll :: MonadIO m => Handle -> [FTPCommand] -> m [FTPResponse]
sendAll = mapM . sendCommand

-- | Equvalent to
--
-- > mapM . sendCommandS
sendAllS :: MonadIO m => Handle -> [FTPCommand] -> m [FTPResponse]
sendAllS = mapM . sendCommandS

-- Control connection

createSocket
    :: MonadIO m
    => Maybe String
    -> Int
    -> S.AddrInfo
    -> m (S.Socket, S.AddrInfo)
createSocket host portNum hints = do
    addr <- liftIO $ do
      a:_ <- S.getAddrInfo (Just hints) host (Just $ show portNum)
      return a
    debugPrint $ "Addr: " <> show addr
    sock <- liftIO $ S.socket
        (S.addrFamily addr)
        (S.addrSocketType addr)
        (S.addrProtocol addr)
    return (sock, addr)

withSocketPassive
    :: (MonadIO m, MonadMask m)
    => String
    -> Int
    -> (S.Socket -> m a)
    -> m a
withSocketPassive host portNum f = do
    let hints = S.defaultHints {
        S.addrSocketType = S.Stream
    }
    M.bracketOnError
        (createSocket (Just host) portNum hints)
        (liftIO . S.close . fst)
        (\(sock, addr) -> do
            debugPrint "Connecting"
            liftIO $ S.connect sock (S.addrAddress addr)
            debugPrint "Connected"
            f sock
        )

withSocketActive :: (MonadIO m, MonadMask m) => (S.Socket -> m a) -> m a
withSocketActive f = do
    let hints = S.defaultHints {
        S.addrSocketType = S.Stream,
        S.addrFlags = [S.AI_PASSIVE]
    }
    M.bracketOnError
        (createSocket Nothing 0 hints)
        (liftIO . S.close . fst)
        (\(sock, addr) -> do
            debugPrint "Binding"
            liftIO $ S.bind sock (S.addrAddress addr)
            liftIO $ S.listen sock 1
            debugPrint "Listening"
            f sock
        )

createSIOHandle :: (MonadIO m, MonadMask m) => String -> Int -> m SIO.Handle
createSIOHandle host portNum = withSocketPassive host portNum
    $ liftIO . flip S.socketToHandle SIO.ReadWriteMode

sIOHandleImpl :: SIO.Handle -> Handle
sIOHandleImpl h = Handle
    { send = C.hPut h
    , sendLine = C.hPutStrLn h
    , recv = C.hGetSome h
    , recvLine = C.hGetLine h
    , security = Clear
    }

withSIOHandle
    :: (MonadIO m, MonadMask m)
    => String
    -> Int
    -> (Handle -> m a)
    -> m a
withSIOHandle host portNum f = M.bracket
    (liftIO $ createSIOHandle host portNum)
    (liftIO . SIO.hClose)
    (f . sIOHandleImpl)

-- | Takes a host name and port. A handle for interacting with the server
-- will be returned in a callback.
--
-- @
-- withFTP "ftp.server.com" 21 $ \h welcome -> do
--     print welcome
--     login h "username" "password"
--     print =<< nlst h []
-- @
withFTP
    :: (MonadIO m, MonadMask m)
    => String
    -> Int
    -> (Handle -> FTPResponse -> m a)
    -> m a
withFTP host portNum f = withSIOHandle host portNum $ \h -> do
    resp <- getResponse h
    f h resp

-- Data connection

withDataSocketPasv
    :: (MonadIO m, MonadMask m)
    => Handle
    -> (S.Socket -> m a)
    -> m a
withDataSocketPasv h f = do
    (host, portNum) <- pasv h
    debugPrint $ "Host: " <> host
    debugPrint $ "Port: " <> show portNum
    withSocketPassive host portNum f

withDataSocketActive
    :: (MonadIO m, MonadMask m)
    => Handle
    -> (S.Socket -> m a)
    -> m a
withDataSocketActive h f = withSocketActive $ \socket -> do
    (sPort, sHost) <- liftIO $ do
      (S.SockAddrInet p h) <- S.getSocketName socket
      return (p,h)
    port h sHost sPort
    f socket

-- | Open a socket that can be used for data transfers
withDataSocket
    :: (MonadIO m, MonadMask m)
    => PortActivity
    -> Handle
    -> (S.Socket -> m a)
    -> m a
withDataSocket Active  = withDataSocketActive
withDataSocket Passive = withDataSocketPasv

acceptData :: MonadIO m => PortActivity -> S.Socket -> m S.Socket
acceptData Passive = return
acceptData Active = return . fst <=< liftIO . S.accept

-- Response to data commands should be 150 but apparently
-- some servers will respond with 200 before 150 so just ignore it
ensureSucessfulData :: MonadIO m => Handle -> FTPResponse -> m ()
ensureSucessfulData h resp = do
    resp' <- case frStatus resp of
        Success -> do
            newResp <- getResponse h
            debugResponse newResp
            return newResp
        _ -> return resp
    liftIO $ when (frStatus resp' /= Wait)
        $ throwIO $ UnsuccessfulException resp

-- | Send setup commands to the server and
-- create a data 'System.IO.Handle'
createSendDataCommand
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> FTPCommand
    -> m SIO.Handle
createSendDataCommand h pa cmd = withDataSocket pa h $ \socket -> do
    resp <- sendCommand h cmd
    ensureSucessfulData h resp
    acceptedSock <- acceptData pa socket
    liftIO $ S.socketToHandle acceptedSock SIO.ReadWriteMode

-- | Provides a data 'Handle' in a callback for a command
withDataCommand
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> RTypeCode
    -> FTPCommand
    -> (Handle -> m a)
    -> m a
withDataCommand ch pa code cmd f = do
    sendCommandS ch $ RType code
    x <- M.bracket
        (createSendDataCommand ch pa cmd)
        (liftIO . SIO.hClose)
        (f . sIOHandleImpl)
    resp <- getResponse ch
    debugResponse resp
    return x

-- | Recieve data and interpret it linewise
getAllLineResp :: (MonadIO m, MonadCatch m) => Handle -> m ByteString
getAllLineResp h = getAllLineResp' h []
    where
        getAllLineResp' h ret = ( do
            line <- liftIO $ getLineResp h
            getAllLineResp' h (ret <> [line]))
                `M.catchIOError` (\_ -> return $ C.intercalate "\n" ret)

-- | Recieve all data and return it as a 'Data.ByteString.ByteString'
recvAll :: (MonadIO m, MonadCatch m) => Handle -> m ByteString
recvAll h = recvAll' ""
    where
        recvAll' bs = ( do
            chunk <- liftIO $ recv h defaultChunkSize
            if C.null chunk
               then return bs
               else recvAll' $ bs <> chunk
            ) `M.catchIOError` (\_ -> return bs)

-- TLS connection

connectTLS :: MonadIO m => SIO.Handle -> String -> Int -> m Connection
connectTLS h host portNum = do
    context <- liftIO initConnectionContext
    let tlsSettings = def
            { settingDisableCertificateValidation = True
            }
        connectionParams = ConnectionParams
            { connectionHostname = host
            , connectionPort = toEnum . fromEnum $ portNum
            , connectionUseSecure = Just tlsSettings
            , connectionUseSocks = Nothing
            }
    liftIO $ connectFromHandle context h connectionParams

createTLSConnection
    :: (MonadIO m, MonadMask m)
    => String
    -> Int
    -> m (FTPResponse, Connection)
createTLSConnection host portNum = do
    h <- createSIOHandle host portNum
    let insecureH = sIOHandleImpl h
    resp <- getResponse insecureH
    sendCommand insecureH Auth
    conn <- connectTLS h host portNum
    return (resp, conn)

tlsHandleImpl :: Connection -> Handle
tlsHandleImpl c = Handle
    { send = connectionPut c
    , sendLine = connectionPut c . (<> "\n")
    , recv = connectionGet c
    , recvLine = connectionGetLine maxBound c
    , security = TLS
    }

withTLSHandle
    :: (MonadMask m, MonadIO m)
    => String
    -> Int
    -> (Handle -> FTPResponse -> m a)
    -> m a
withTLSHandle host portNum f = M.bracket
    (createTLSConnection host portNum)
    (liftIO . connectionClose . snd)
    (\(resp, conn) -> f (tlsHandleImpl conn) resp)

-- | Takes a host name and port. A handle for interacting with the server
-- will be returned in a callback. The commands will be protected with TLS.
--
-- @
-- withFTPS "ftps.server.com" 21 $ \h welcome -> do
--     print welcome
--     login h "username" "password"
--     print =<< nlst h []
-- @
withFTPS
    :: (MonadMask m, MonadIO m)
    => String
    -> Int
    -> (Handle -> FTPResponse -> m a)
    -> m a
withFTPS = withTLSHandle

-- TLS data connection

-- | Send setup commands to the server and
-- create a data TLS connection
createTLSSendDataCommand
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> FTPCommand
    -> m Connection
createTLSSendDataCommand ch pa cmd = do
    sendAllS ch [Pbsz 0, Prot P]
    withDataSocket pa ch $ \socket -> do
        resp <- sendCommand ch cmd
        ensureSucessfulData ch resp
        acceptedSock <- acceptData pa socket
        (sPort, sHost) <- liftIO $ do
          (S.SockAddrInet p h) <- S.getSocketName acceptedSock
          return (p, h)
        let (h1, h2, h3, h4) = S.hostAddressToTuple sHost
            hostName = intercalate "." $ show . fromEnum <$> [h1, h2, h3, h4]
        h <- liftIO $ S.socketToHandle acceptedSock SIO.ReadWriteMode
        liftIO $ connectTLS h hostName (fromEnum sPort)

withTLSDataCommand
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> RTypeCode
    -> FTPCommand
    -> (Handle -> m a)
    -> m a
withTLSDataCommand ch pa code cmd f = do
    sendCommandS ch $ RType code
    x <- M.bracket
        (createTLSSendDataCommand ch pa cmd)
        (liftIO . connectionClose)
        (f . tlsHandleImpl)
    resp <- getResponse ch
    debugPrint $ "Recieved: " <> show resp
    return x

parseResponse :: MonadIO m => FTPResponse -> Parser a -> m a
parseResponse resp p =
    let parsableMessage = case frMessage resp of
            SingleLine message -> message
            MultiLine messages -> C.intercalate "\n" messages
    in case parseOnly p parsableMessage of
        Right x -> return x
        Left _ -> liftIO $ throwIO
            $ BadProtocolResponseException parsableMessage

ensureCode :: MonadIO m => FTPResponse -> Int -> m ()
ensureCode resp code =
    liftIO $ when (frCode resp /= code)
        $ liftIO $ throwIO $ UnsuccessfulException resp

parse227 :: Parser (String, Int)
parse227 = do
    skipWhile (/= '(') *> char '('
    [h1,h2,h3,h4,p1,p2] <- many1 digit `sepBy` char ','
    let host = intercalate "." [h1,h2,h3,h4]
        highBits = read p1
        lowBits = read p2
        portNum = (highBits `shift` 8) + lowBits
    return (host, portNum)

parse257 :: Parser String
parse257 = do
    char '"'
    C.unpack <$> takeTill (== '"')

-- Control commands

login :: MonadIO m => Handle -> String -> String -> m FTPResponse
login h user pass = do
    resp <- last <$> sendAll h [User user, Pass pass]
    ensureSuccess resp

pasv :: MonadIO m => Handle -> m (String, Int)
pasv h = do
    resp <- sendCommandS h Pasv
    ensureCode resp 227
    parseResponse resp parse227

port :: MonadIO m => Handle -> S.HostAddress -> S.PortNumber -> m FTPResponse
port h ha pn = sendCommandS h (Port ha pn)

acct :: MonadIO m => Handle -> String -> m FTPResponse
acct h pass = sendCommandS h (Acct pass)

rename :: MonadIO m => Handle -> String -> String -> m FTPResponse
rename h from to = do
    res <- sendCommand h (Rnfr from)
    case frStatus res of
        Continue -> sendCommandS h (Rnto to)
        _ -> return res

dele :: MonadIO m => Handle -> String -> m FTPResponse
dele h file = sendCommandS h (Dele file)

cwd :: MonadIO m => Handle -> String -> m FTPResponse
cwd h dir =
    sendCommandS h $ if dir == ".."
        then Cdup
        else Cwd dir

size :: MonadIO m => Handle -> String -> m Int
size h file = do
    resp <- sendCommandS h (Size file)
    ensureCode resp 213
    return $ case frMessage resp of
        SingleLine message -> read . C.unpack $ message
        MultiLine _ -> 0

mkd :: MonadIO m => Handle -> String -> m String
mkd h dir = do
    resp <- sendCommandS h (Mkd dir)
    ensureCode resp 257
    parseResponse resp parse257

rmd :: MonadIO m => Handle -> String -> m FTPResponse
rmd h dir = sendCommandS h (Rmd dir)

pwd :: MonadIO m => Handle -> m String
pwd h = do
    resp <- sendCommandS h Pwd
    ensureCode resp 257
    parseResponse resp parse257

quit :: MonadIO m => Handle -> m FTPResponse
quit h = sendCommandS h Quit

mlst :: (MonadIO m, MonadMask m) => Handle -> String -> m MlsxResponse
mlst h path = do
    resp <- sendCommandS h (Mlst path)
    case frMessage resp of
        SingleLine message -> return $ parseMlsxLine message
        MultiLine messages -> if length messages >= 2
            then return $ parseMlsxLine $ messages !! 1
            else liftIO $ throwIO $ BogusResponseFormatException resp

-- TLS commands

pbsz :: MonadIO m => Handle -> Int -> m FTPResponse
pbsz h = sendCommandS h . Pbsz

prot :: MonadIO m => Handle -> ProtType -> m FTPResponse
prot h = sendCommandS h . Prot

ccc :: MonadIO m => Handle -> m FTPResponse
ccc h = sendCommandS h Ccc

auth :: MonadIO m => Handle -> m FTPResponse
auth h = sendCommandS h Auth

-- Data commands

sendType :: MonadIO m => RTypeCode -> ByteString -> Handle -> m ()
sendType TA dat h = mapM_ (sendCommandLine h) $ C.split '\n' dat
sendType TI dat h = liftIO $ send h dat

withDataCommandSecurity
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> RTypeCode
    -> FTPCommand
    -> (Handle -> m a)
    -> m a
withDataCommandSecurity h =
    case security h of
        Clear -> withDataCommand h
        TLS -> withTLSDataCommand h

nlst :: (MonadIO m, MonadMask m) => Handle -> [String] -> m ByteString
nlst h args = withDataCommandSecurity h Passive TA (Nlst args) getAllLineResp

retr :: (MonadIO m, MonadMask m) => Handle -> String -> m ByteString
retr h path = withDataCommandSecurity h Passive TI (Retr path) recvAll

list :: (MonadIO m, MonadMask m) => Handle -> [String] -> m ByteString
list h args = withDataCommandSecurity h Passive TA (List args) getAllLineResp

stor
    :: (MonadIO m, MonadMask m)
    => Handle
    -> String
    -> B.ByteString
    -> RTypeCode
    -> m ()
stor h loc dat rtype =
    withDataCommandSecurity h Passive rtype (Stor loc) $ sendType rtype dat

data MlsxResponse = MlsxResponse {
    mrFilename :: String,
    mrFacts :: Map String String
} deriving (Show)

splitApart :: Char -> ByteString -> (ByteString, ByteString)
splitApart on s =
    let (x0, x1) = C.break (== on) s
    in (x0, C.drop 1 x1)

parseMlsxLine :: ByteString -> MlsxResponse
parseMlsxLine line =
    let (factLine, filename) = splitApart ' ' line
        bFacts = splitApart '=' <$> C.split ';' factLine
        facts
            = Map.fromList
            $ filter (not . null . fst)
            $ join (***) C.unpack <$> bFacts
    in MlsxResponse (C.unpack filename) facts

getMlsxResponse :: (MonadIO m, MonadCatch m) => Handle -> m [MlsxResponse]
getMlsxResponse h = getMlsxResponse' h []
    where
        getMlsxResponse' h ret = ( do
            line <- liftIO $ getLineResp h
            getMlsxResponse' h $
                if C.null line
                    then ret
                    else parseMlsxLine line : ret
            ) `M.catchIOError` (\_ -> return ret)

mlsd :: (MonadIO m, MonadMask m) => Handle -> String -> m [MlsxResponse]
mlsd h path = withDataCommandSecurity h Passive TA (Mlsd path) getMlsxResponse
