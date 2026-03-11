module Katip.Wai.Middleware
  ( ApplicationT
  , MiddlewareT
  , runApplication
  , middlewareCustom
  , middleware
  ) where

import Katip.Wai.Options (Options)
import qualified Katip.Wai.Options as Options
import qualified Katip.Wai.Request as Request
import qualified Katip.Wai.Response as Response

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Katip
import qualified Network.Wai as Wai


-- | Just like 'Wai.Application' except it runs in @m@ instead of 'IO'
type ApplicationT m = Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived


-- | Just like 'Wai.Middleware' except it runs in @m@ instead of 'IO'
type MiddlewareT m = ApplicationT m -> ApplicationT m


-- | Converts an 'ApplicationT' to a normal 'Wai.Application'
runApplication :: MonadIO m => (forall a. m a -> IO a) -> ApplicationT m -> Wai.Application
runApplication toIO application request send =
  toIO $ application request (liftIO . send)


-- | Same as 'middleware', but allows you to customize how the 'Request.Request'
-- and 'Response.Response' are handled.
middlewareCustom
  :: MonadIO m
  => Options m
  -> MiddlewareT m
middlewareCustom options application request send = do
  tracedRequest <- Request.traceRequest request
  Options.handleRequest options tracedRequest $
    application request $ \response -> do
      tracedResponse <- Response.traceResponse tracedRequest response
      Options.handleResponse options tracedResponse $
        send response


-- | Add the request and response to the 'Katip.LogContexts', and log a message
-- when a request is received and when a response is sent.
--
-- This uses the default format: 'Options.defaultRequestFormat' and 'Options.defaultResponseFormat' with milliseconds for the response time.
--
-- If you want more customization see 'middlewareCustom'.
middleware :: Katip.KatipContext m => Katip.Severity -> MiddlewareT m
middleware =
  middlewareCustom . Options.defaultOptions
