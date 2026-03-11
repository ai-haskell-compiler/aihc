-- | Module for using a WAI Middleware as an X-Ray client
module Network.AWS.XRayClient.WAI
  ( XRayClientConfig(..)
  , xrayClientConfig
  , xrayTraceMiddleware
  , NoAddressInfoException(..)
  , xrayWaiVaultKey
  , vaultDataFromRequest
  , XRayVaultData(..)
  , traceXRaySubsegment
  , traceXRaySubsegment'
  , atomicallyAddVaultDataSubsegment
  , makeSubsegmentIndependent
  , module X
  ) where

import Prelude

import Control.Concurrent (forkIO)
import Control.Lens hiding ((|>))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (toList)
import Data.IORef
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import qualified Data.Vault.Lazy as V
import Network.AWS.XRayClient.Segment as X
import Network.AWS.XRayClient.SendSegments as X
import Network.AWS.XRayClient.TraceId as X
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import System.Random.XRayCustom
import UnliftIO.Exception (finally)

-- | Configuration type for the XRay client middleware.
data XRayClientConfig = XRayClientConfig
  { xrayClientConfigDaemonHost :: !Text
  -- ^ The host that the daemon is listening on.
  , xrayClientConfigDaemonPort :: !Int
  -- ^ The port that the daemon is listening on.
  , xrayClientConfigApplicationName :: !Text
  -- ^ The value of the "name" field that will be sent to X-Ray.
  , xrayClientConfigSampler
      :: !(Maybe (Request -> Response -> POSIXTime -> POSIXTime -> IO Bool))
  -- ^ A sampling function to filter traces.
  }

-- | Constructor for 'XRayClientConfig' with required arguments.
xrayClientConfig :: Text -> XRayClientConfig
xrayClientConfig appName = XRayClientConfig
  { xrayClientConfigDaemonHost = "127.0.0.1"
  , xrayClientConfigDaemonPort = 2000
  , xrayClientConfigApplicationName = appName
  , xrayClientConfigSampler = Nothing
  }

-- | Traces the execution time of a request and sends the the local X-Ray
-- daemon.
xrayTraceMiddleware :: XRayClientConfig -> Middleware
xrayTraceMiddleware clientConfig@XRayClientConfig {..} app request respond = do
  -- Start timer. We purposely include all of this setup in the time.
  startTime <- getPOSIXTime

  -- Create an IORef StdGen. We share this across the life of the request so we
  -- don't have multiple simultaneous requests causing contention on the global
  -- StdGen.
  stdGenIORef <- newStdGen >>= newIORef

  -- Check for the X-Amzn-Trace-Id and try to parse it. If that isn't possible,
  -- then make a new header.
  let
    oldHeaders = requestHeaders request
    mTraceIdHeaderData =
      lookup amazonTraceIdHeaderName oldHeaders >>= parseXRayTraceIdHeaderData
  (newHeaders, headerData@XRayTraceIdHeaderData {..}) <-
    case mTraceIdHeaderData of
      Just headerData -> pure (oldHeaders, headerData)
      Nothing -> do
        traceId <- generateXRayTraceId stdGenIORef
        let
          headerData = xrayTraceIdHeaderData traceId
          header =
            (amazonTraceIdHeaderName, makeXRayTraceIdHeaderValue headerData)
        pure (header : oldHeaders, headerData)

  -- Construct the new request including the vault data
  segmentId <- withRandomGenIORef stdGenIORef generateXRaySegmentId
  subsegmentsIORef <- newIORef Seq.empty
  let
    vaultData = XRayVaultData
      headerData
      clientConfig
      segmentId
      stdGenIORef
      subsegmentsIORef
    request' = request
      { requestHeaders = newHeaders
      , vault = V.insert xrayWaiVaultKey vaultData (vault request)
      }

  -- Run application with the new Request
  app request' $ \response -> do
    responseAccept <- respond response

    -- Create segment
    endTime <- getPOSIXTime
    subsegments <- readIORef subsegmentsIORef
    let
      segment =
        xraySegment
            xrayClientConfigApplicationName
            segmentId
            xrayTraceIdHeaderDataRootTraceId
            startTime
            (Just endTime)
          & xraySegmentParentId
          .~ xrayTraceIdHeaderDataParentId
          & addRequestToSegment request'
          & addResponseToSegment response
      independentSubsegments =
        makeSubsegmentIndependent vaultData <$> toList subsegments

    -- Send the segment if it passes the filters
    shouldSend <- maybe
      (pure True)
      (\f -> f request response startTime endTime)
      xrayClientConfigSampler
    when shouldSend $ void $ forkIO $ sendSegmentsToDaemon
      xrayClientConfigDaemonHost
      xrayClientConfigDaemonPort
      (segment : independentSubsegments)

    -- Be a good middleware and return the original ResponseAccept
    pure responseAccept

-- | Adds the info from a WAI 'Request' to an 'XRaySegment'.
addRequestToSegment :: Request -> XRaySegment -> XRaySegment
addRequestToSegment request =
  xraySegmentHttp
    . non xraySegmentHttpDef
    . xraySegmentHttpRequest
    . non xraySegmentHttpRequestDef
    %~ ((xraySegmentHttpRequestMethod
        ?~ T.pack (BS8.unpack (requestMethod request))
        )
       . (xraySegmentHttpRequestUrl ?~ "/" <> T.intercalate
           "/"
           (pathInfo request)
         )
       . (xraySegmentHttpRequestUserAgent
         .~ (T.pack . BS8.unpack <$> requestHeaderUserAgent request)
         )
       )

-- | Adds the info from a WAI 'Response' to an 'XRaySegment'.
addResponseToSegment :: Response -> XRaySegment -> XRaySegment
addResponseToSegment response =
  xraySegmentHttp
    . non xraySegmentHttpDef
    . xraySegmentHttpResponse
    . non xraySegmentHttpResponseDef
    %~ (xraySegmentHttpResponseStatus ?~ statusCode (responseStatus response))

-- | We use the WAI 'V.Vault' to store data needed during traces.
data XRayVaultData = XRayVaultData
  { xrayVaultDataTraceIdHeaderData :: !XRayTraceIdHeaderData
  -- ^ Data about the current trace.
  , xrayVaultDataClientConfig :: !XRayClientConfig
  -- ^ Client configuration passed into the middleware.
  , xrayVaultDataRootSegmentId :: !XRaySegmentId
  -- ^ Segment ID of the root segment for this request.
  , xrayVaultDataStdGen :: !(IORef StdGen)
  -- ^ 'StdGen' for generating segment IDs and trace IDs.
  , xrayVaultDataSubsegments :: !(IORef (Seq XRaySegment))
  -- ^ Current list of subsegments.
  }

-- | This is a 'V.Key' for the @vault@ inside each WAI 'Request'. It is used to
-- get to the 'XRayVaultData' for the current request.
xrayWaiVaultKey :: V.Key XRayVaultData
xrayWaiVaultKey = unsafePerformIO V.newKey
{-# NOINLINE xrayWaiVaultKey #-}

-- | Try to get 'XRayVaultData' from the WAI 'Request' vault.
vaultDataFromRequest :: Request -> Maybe XRayVaultData
vaultDataFromRequest = V.lookup xrayWaiVaultKey . vault

-- | Time a 'MonadIO' action and add it to the list of subsegments.
traceXRaySubsegment
  :: MonadUnliftIO m
  => Request
  -> Text
  -> (XRaySegment -> XRaySegment)
  -> m a
  -> m a
traceXRaySubsegment request subsegmentName modifySubsegment action =
  case V.lookup xrayWaiVaultKey (vault request) of
    Nothing -> action
    Just v -> traceXRaySubsegment' v subsegmentName modifySubsegment action

traceXRaySubsegment'
  :: MonadUnliftIO m
  => XRayVaultData
  -> Text
  -> (XRaySegment -> XRaySegment)
  -> m a
  -> m a
traceXRaySubsegment' vaultData subsegmentName modifySubsegment action = do
  -- Run action with timing
  startTime <- liftIO getPOSIXTime

  -- Catch any exceptions and rethrow them once we've sent the segment
  finally action $ liftIO $ do
    endTime <- getPOSIXTime

    segmentId <- withRandomGenIORef
      (xrayVaultDataStdGen vaultData)
      generateXRaySegmentId

    let
      subsegment =
        xraySubsegment subsegmentName segmentId startTime (Just endTime)
      subsegment' = modifySubsegment subsegment
    atomicallyAddVaultDataSubsegment vaultData subsegment'

-- | Add subsegment to XRay vault data 'IORef'.
atomicallyAddVaultDataSubsegment :: XRayVaultData -> XRaySegment -> IO ()
atomicallyAddVaultDataSubsegment vaultData subsegment =
  atomicModifyIORef' (xrayVaultDataSubsegments vaultData)
    $ \subsegments -> (subsegments |> subsegment, ())

-- | Uses the trace ID and segment ID of the root segment from the vault to
-- make a subsegment independent. This is useful so nested components that
-- create subsegments don't need all of this information threaded down to them.
-- We can just decorate all of the subsegments with it before sending them off.
makeSubsegmentIndependent :: XRayVaultData -> XRaySegment -> XRaySegment
makeSubsegmentIndependent XRayVaultData {..} subsegment =
  subsegment
    & xraySegmentTraceId
    ?~ xrayTraceIdHeaderDataRootTraceId xrayVaultDataTraceIdHeaderData
    & xraySegmentParentId
    ?~ xrayVaultDataRootSegmentId
    & xraySegmentType
    ?~ "subsegment"
