{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- | Miscellaneous HTTP Utilities. -}
module OM.HTTP (
  runTlsRedirect,
  hstsDirective,
  requestLogging,
  setServer,
  insertResponseHeaderIfMissing,
  overwriteResponseHeader,
  staticSite,
  logExceptionsAndContinue,
  sshConnect,
  staticPage,
  defaultIndex,
  BearerToken(..),
  emptyApp,
) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Exception (someExceptionContext, try)
import Control.Exception.Context (displayExceptionContext)
import Control.Exception.Safe (SomeException, bracket, finally, throwM, tryAny)
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger.Aeson
  ( LoggingT(runLoggingT), Message((:#)), (.=), Loc, LogLevel, LogSource, LogStr
  , MonadLoggerIO, logError, logInfo
  )
import Data.Aeson (object)
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encodeBase64)
import Data.List ((\\))
import Data.Maybe (catMaybes)
import Data.Set (Set, member)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Data.Version (Version, showVersion)
import Language.Haskell.TH (Code(examineCode), Q, TExp, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import Network.HTTP.Types
  ( Status(statusCode, statusMessage), Header, HeaderName
  , internalServerError500, methodNotAllowed405, movedPermanently301, ok200
  , status404
  )
import Network.Mime (defaultMimeLookup)
import Network.Socket
  ( AddrInfo(addrAddress), Family(AF_INET), SocketType(Stream), Socket, close
  , connect, defaultProtocol, getAddrInfo, socket
  )
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai
  ( Request
    ( isSecure, pathInfo, rawPathInfo, rawQueryString, remoteHost
    , requestHeaders, requestMethod
    )
  , Application, Middleware, Response, ResponseReceived, mapResponseHeaders
  , responseHeaders, responseLBS, responseRaw, responseStatus
  )
import Network.Wai.Handler.Warp (run)
import OM.Show (showt)
import Prelude
  ( Applicative(pure), Either(Left, Right), Eq((/=), (==))
  , Foldable(elem, foldr), Functor(fmap), Maybe(Just, Nothing)
  , Monad((>>), (>>=), return), MonadFail(fail), Monoid(mempty)
  , RealFrac(truncate), Semigroup((<>)), Show(show), Traversable(mapM), ($)
  , (++), (.), (<$>), (=<<), FilePath, IO, Int, String, concat, drop, filter
  , fst, id, mapM_, otherwise, putStrLn, seq, zip
  )
import Servant.API (ToHttpApiData(toUrlPiece))
import System.Directory (getDirectoryContents)
import System.FilePath.Posix ((</>), combine)
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T


{- |
  Runs a web server on port 80, that redirects to the given url. Does
  request logging, and sets the HSTS Directive header, and in the unlikely
  event of excptions it will also catch and log them.
-}
runTlsRedirect
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ Logging backend. -}
  -> ByteString {- ^ Server name. -}
  -> Version {- ^ Server version. -}
  -> ByteString {- ^ Target URL. -}
  -> IO ()
runTlsRedirect logging serverName serverVersion url =
  run 80
    . requestLogging logging mempty
    . setServer serverName serverVersion
    . hstsDirective 600
    . logExceptionsAndContinue logging
    $ tlsRedirect url


{- |
  Inject the HSTS directives, see
  https://en.wikipedia.org/wiki/HTTP_Strict_Transport_Security.
-}
hstsDirective :: NominalDiffTime -> Middleware
hstsDirective age = insertResponseHeaderIfMissing header
  where
    header :: Header
    header =
      ("Strict-Transport-Security", "max-age=" <> showt (truncate age :: Int))


{- | Insert a response header only if it has not already been inserted. -}
insertResponseHeaderIfMissing :: Header -> Middleware
insertResponseHeaderIfMissing (name, val) app req respond =
    app req (respond . mapResponseHeaders doInsert)
  where
    doInsert :: [Header] -> [Header]
    doInsert headers
      | name `elem` (fst <$> headers) = headers
      | otherwise = (name, val):headers


{- |
  TLS redirect. An 'Application' that redirects unsecured requests to
  the secure HTTPS site.
-}
tlsRedirect :: ByteString -> Application
tlsRedirect url _req respond = respond $
  responseLBS
    movedPermanently301
    [
      ("Location", url),
      ("Content-Type", "text/html")
    ]
    (
      "<html>\
        \<head>\
        \</head>\
        \<body>\
          \Please use our secure site,\
          \<a href=\"" <> BSL.fromStrict url <> "\">here</a>\
        \</body>\
      \</html>"
    )


{- | Set the @Server:@ header. -}
setServer :: ByteString -> Version -> Middleware
setServer serviceName version =
    overwriteResponseHeader ("Server", serverValue)
  where
    {- | The value of the @Server:@ header. -}
    serverValue = serviceName <> "/" <> fromString (showVersion version)


{- |
  Inserts a response header, clobbering any and all existing values for
  the given header.
-}
overwriteResponseHeader :: Header -> Middleware
overwriteResponseHeader (name, value) app req respond =
    app req (respond . mapResponseHeaders go)
  where
    go :: [Header] -> [Header]
    go headers =
      (name, value) : filter ((/= name) . fst) headers


{- |
  Logs an HTTP request by emitting two log messages. The first messages
  logs that the request has begun. The second messages logs the status
  result and timing of the request once it is finished.

  > {"text":"Starting request: GET /api/me","meta":{..<request details>..}}
  > {"text":"Request complete: GET /api/me","meta":{"status":"200 OK","time":"0.000370671s"}}

  This can help debugging requests that hang or crash for whatever reason.
-}
requestLogging
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Set HeaderName {-^ Headers to redact -}
  -> Middleware
requestLogging logging redactHeaders app req respond =
    (`runLoggingT` logging) $ do
      logInfo $ "Starting request: " <> reqStr :#
        [ "method" .= decodeUtf8Safe (requestMethod req)
        , "path" .= decodeUtf8Safe (rawPathInfo req)
        , "query" .= decodeUtf8Safe (rawQueryString req)
        , "secure" .= isSecure req
        , "remote_host" .= show (remoteHost req)
        , "headers" .=
            fmap
              (\(name, val) ->
                decodeUtf8Safe (CI.original name)
                <> ": "
                <> if name `member` redactHeaders
                     then "<redacted>"
                     else decodeUtf8Safe val
              )
              (requestHeaders req)
        ]
      now <- liftIO getCurrentTime
      liftIO (try (app req (loggingRespond now))) >>= \case
        Left err -> do
          logError $
            "`app` failed with exception. The exception will be re-thrown." :#
            [ "exception" .= show (err :: SomeException)
            , "context" .= displayExceptionContext (someExceptionContext err)
            ]
          throwM err
        Right val -> pure val
  where
    {- | Delegate to the underlying responder, and do some logging. -}
    loggingRespond :: UTCTime -> Response -> IO ResponseReceived
    loggingRespond start response =
      (`runLoggingT` logging) $ do
        {-
          Execute the underlying responder first so we get an accurate
          measurement of the request duration.
        -}
        ack <- liftIO (try (respond response)) >>= \case
          Left err -> do
            logError $
              "`respond` failed with exception. The exception will be re-thrown." :#
              [ "exception" .= show (err :: SomeException)
              , "context" .= displayExceptionContext (someExceptionContext err)
              , "response" .= object
                  [ "status" .= showStatus (responseStatus response)
                  , "headers" .=
                      fmap
                        (\(name, val) ->
                          decodeUtf8Safe (CI.original name)
                          <> ": "
                          <> decodeUtf8Safe val
                        )
                        (responseHeaders response)
                  ]
              ]
            throwM err
          Right ack -> pure ack
        now <- ack `seq` liftIO getCurrentTime
        logInfo $ "Request complete: " <> reqStr :#
          [ "status" .= showStatus (responseStatus response)
          , "time" .= show (diffUTCTime now start)
          ]
        return ack

    {- | A Text representation of the request, suitable for logging. -}
    reqStr :: Text
    reqStr =
      decodeUtf8Safe
        (requestMethod req <> " " <> rawPathInfo req <> rawQueryString req)

    {- |
      @instance Show Status@ shows the Haskell structure, which is
      not suitable for logging.
    -}
    showStatus :: Status -> Text
    showStatus stat =
      (showt . statusCode) stat <> " " <> (decodeUtf8Safe . statusMessage) stat


{- |
  Logs all exceptions, and returns a 500 Internal Server error.

  This is useful because your wai framework won't always do what you
  expect when it encounters random exceptions. For instance, an exception
  thrown in IO may cause functionality of higher-level middlewares to be
  bypassed unless they know how to catch and re-throw exceptions (making
  them more complicated). This middleware explicitly will not re-throw
  exceptions, unless those exceptions were encountered after the headers
  have already been sent, e.g. when using 'Network.Wai.StreamingBody'.

  What it will do is generate a unique id for the exception and print
  that ID, so you can easily find it in the logs.
-}
logExceptionsAndContinue
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ Logging backend. -}
  -> Middleware
logExceptionsAndContinue logging app req respond = (`runLoggingT` logging) $
    tryAny (liftIO (app req loggingRespond)) >>= \case
      Right ack -> return ack
      Left err -> do
        uuid <- logProblem err
        liftIO $ respond (errResponse uuid)
  where
    errResponse :: UUID -> Response
    errResponse uuid =
      responseLBS
        internalServerError500
        [("Content-Type", "text/plain")]
        ("Internal Server Error. Error ID: " <> showt uuid)

    getUUID :: (MonadIO m) => m UUID
    getUUID = liftIO nextUUID >>= \case
      Nothing -> liftIO (threadDelay 1000) >> getUUID
      Just uuid -> return uuid

    loggingRespond :: Response -> IO ResponseReceived
    loggingRespond response = (`runLoggingT` logging) $
      tryAny (liftIO (respond response)) >>= \case
        Right ack -> return ack
        Left err -> do
          void $ logProblem err
          throwM err

    logProblem :: (MonadLoggerIO m) => SomeException -> m UUID
    logProblem err = do
      uuid <- getUUID
      logError $ "Internal Server Error" :#
        [ "exception" .= show err
        , "context" .= displayExceptionContext (someExceptionContext err)
        , "id" .= uuid
        ]
      return uuid


{- |
  'Middleware' that provides an HTTP @CONNECT@ passthrough to the local
  ssh port. Useful primarily for bypassing content-inspection firewalls.
-}
sshConnect :: Middleware
sshConnect app req respond =
    case requestMethod req of
      "CONNECT" ->
        respond (responseRaw connProxy (responseLBS methodNotAllowed405 [] ""))
      _ -> app req respond
  where
    {- |
      Open a connection to the local ssh port and mediate the traffic between
      that service and the client.
    -}
    connProxy :: IO ByteString -> (ByteString -> IO ()) -> IO ()
    connProxy read_ write =
      bracket
        (socket AF_INET Stream defaultProtocol)
        (\so ->  close so `finally` write "")
        (\so -> do
          connect so =<<
            (
              getAddrInfo Nothing (Just "127.0.0.1") (Just "22") >>= \case
                [] -> fail "Address not found: 127.0.0.1:22"
                sa:_ -> return (addrAddress sa)
            )
          concurrently_
            (pipeInbound so read_)
            (pipeOutbound so write)
        )

    {- | Forward data coming from the client, going to the ssh service. -}
    pipeInbound :: Socket -> IO ByteString -> IO ()
    pipeInbound so read_ = do
      bytes <- read_
      if BS.null bytes
        then return ()
        else do
          sendAll so bytes
          pipeInbound so read_

    {- | Forward data coming from the ssh service, going to the client. -}
    pipeOutbound :: Socket -> (ByteString -> IO ()) -> IO ()
    pipeOutbound so write = do
      bytes <- recv so 4096
      write bytes
      if BS.null bytes
        then return ()
        else pipeOutbound so write


{- | Serve a static page at the given 'pathInfo'. -}
staticPage
  :: [Text] {- ^ The path info. -}
  -> ByteString {- ^ The content type. -}
  -> BSL.ByteString {- ^ The response body content. -}
  -> Middleware
staticPage path ct bytes app req respond =
  if pathInfo req == path
    then respond (responseLBS ok200 [("Content-Type", ct)] bytes)
    else app req respond


{- | Rewrite: "\/" -> "/index.html". -}
defaultIndex :: Middleware
defaultIndex app request respond =
  case pathInfo request of
    [] -> app request {pathInfo = ["index.html"]} respond
    _ -> app request respond


{- |
  A bearer token, which is an instance of the necessary type classes to
  be useful as a servant header value.
-}
newtype BearerToken = BearerToken {
    unBearerToken :: Text
  }
instance ToHttpApiData BearerToken where
  toUrlPiece t = "Bearer " <> unBearerToken t


{- |
  The Template-Haskell splice @$$(staticSite dir)@ will build a
  'Middleware' that serves a set of static files determined at
  compile time, or else passes the request to the underlying
  'Network.Wai.Application'.

  All files under @dir@ will be served relative to the root path of
  your web server, so the file @\<dir\>\/foo\/bar.html@ will be served at
  @http://your-web-site.com/foo/bar.html@

  The content-type of the files being served will be guessed using
  'defaultMimeLookup'.
-}
staticSite :: FilePath -> Q (TExp Middleware)
staticSite baseDir = join . runIO $ do
    files <- readStaticFiles
    mapM_ (printResource . fst) files
    return $ mapM_ (addDependentFile . ((baseDir ++ "/") ++) . fst) files >> examineCode [||
        let
          {- |
            Build a middleware that serves a single static file path, or
            delegates to the underlying application.
          -}
          static :: (FilePath, String) -> Middleware
          static (filename, content) app req respond =
            let
              {- | Guess the content type of the static file. -}
              ct :: ByteString
              ct =
                defaultMimeLookup
                . fromString
                $ filename
            in
              if pathInfo req == T.split (== '/') (T.pack filename)
                then
                  respond (
                      responseLBS
                        ok200
                        [("content-type", ct)]
                        (BSL8.pack content)
                    )
                else app req respond
        in
          foldr ((.) . static) id files :: Middleware
      ||]
  where
    printResource :: String -> IO ()
    printResource file =
      putStrLn ("Generating static resource for: " ++ show file)

    {- | Reads the static files that make up the admin user interface. -}
    readStaticFiles :: IO [(FilePath, String)]
    readStaticFiles =
      let
        findAll :: FilePath -> IO [FilePath]
        findAll dir = do
            contents <-
              (\\ [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
            dirs <- catMaybes <$> mapM justDir contents
            files <- catMaybes <$> mapM justFile contents
            more <- concat <$> mapM (findAll . combine dir) dirs
            return $ (combine dir <$> files) ++ more
          where
            justFile :: FilePath -> IO (Maybe FilePath)
            justFile filename = do
              isfile <-
                isRegularFile <$>
                  getFileStatus (baseDir </> dir </> filename)
              return $ if isfile then Just filename else Nothing

            justDir :: FilePath -> IO (Maybe FilePath)
            justDir filename = do
              isdir <-
                isDirectory <$>
                  getFileStatus (baseDir </> dir </> filename)
              return $ if isdir then Just filename else Nothing
      in do
        allFiles <- findAll "."
        allContent
          <- mapM (fmap BS8.unpack . BS.readFile . combine baseDir) allFiles
        return (zip (drop 2 <$> allFiles) allContent)


{-| A WAI 'Application' that returns 404 not found for everything. -}
emptyApp :: Application
emptyApp _req respond =
  respond
    (
      responseLBS
        status404
        mempty
        "not found"
    )


{- | Safely decode UTF-8 bytes to Text, replacing invalid sequences with placeholder. -}
decodeUtf8Safe :: ByteString -> Text
decodeUtf8Safe bytes =
  case decodeUtf8' bytes of
    Left _err -> "<invalid utf8:" <> extractBase64 (encodeBase64 bytes) <> ">"
    Right val -> val


