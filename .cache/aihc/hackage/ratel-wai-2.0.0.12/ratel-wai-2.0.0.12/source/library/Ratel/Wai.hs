module Ratel.Wai
  ( ratelMiddleware,
    toRequest,
  )
where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Network.HTTP.Client as Client
import qualified Network.Wai as Wai
import qualified Ratel

ratelMiddleware ::
  Ratel.ApiKey ->
  Maybe Client.Manager ->
  (Ratel.Payload -> IO Ratel.Payload) ->
  Wai.Middleware
ratelMiddleware apiKey maybeManager modify handle request respond = do
  Exception.catch
    ( do
        handle
          request
          ( \response -> do
              respond response
          )
    )
    ( \exception -> do
        let err = Ratel.toError (exception :: Exception.SomeException)
        let req = toRequest request
        let server =
              Ratel.Server
                { Ratel.serverEnvironmentName = Nothing,
                  Ratel.serverHostname = Nothing,
                  Ratel.serverProjectRoot = Nothing
                }
        payload <-
          modify
            Ratel.Payload
              { Ratel.payloadError = err,
                Ratel.payloadNotifier = Nothing,
                Ratel.payloadRequest = Just req,
                Ratel.payloadServer = server
              }
        _ <-
          Concurrent.forkIO
            ( do
                _ <- Ratel.notify apiKey maybeManager payload
                pure ()
            )
        Exception.throwIO exception
    )

toRequest :: Wai.Request -> Ratel.Request
toRequest request =
  Ratel.Request
    { Ratel.requestAction = Nothing,
      Ratel.requestCgiData =
        Just
          ( Map.union
              ( Map.fromList
                  [ ("REMOTE_ADDR", show (Wai.remoteHost request)),
                    ("REQUEST_METHOD", BS.unpack (Wai.requestMethod request))
                  ]
              )
              ( Map.fromList
                  ( fmap
                      ( \(k, v) ->
                          ( "HTTP_"
                              <> fmap
                                (\c -> if c == '-' then '_' else Char.toUpper c)
                                (BS.unpack (CI.foldedCase k)),
                            BS.unpack v
                          )
                      )
                      (Wai.requestHeaders request)
                  )
              )
          ),
      Ratel.requestComponent = Nothing,
      Ratel.requestContext = Nothing,
      Ratel.requestParams =
        Just
          ( Map.fromList
              ( fmap
                  (Bifunctor.bimap BS.unpack (maybe "" BS.unpack))
                  (Wai.queryString request)
              )
          ),
      Ratel.requestSession = Nothing,
      Ratel.requestUrl =
        Just
          ( BS.unpack (Wai.rawPathInfo request)
              <> BS.unpack (Wai.rawQueryString request)
          )
    }
