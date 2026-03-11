{-# LANGUAGE FlexibleContexts #-}

module Network.AWS.XRayClient.TraceId
  ( amazonTraceIdHeaderName
  , -- * Trace ID
    XRayTraceId(..)
  , generateXRayTraceId
  , makeXRayTraceId
  , XRaySegmentId(..)
  , generateXRaySegmentId
    -- * Trace ID Header
  , XRayTraceIdHeaderData(..)
  , xrayTraceIdHeaderData
  , parseXRayTraceIdHeaderData
  , makeXRayTraceIdHeaderValue
  ) where

import Prelude

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (intToDigit)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics
import Network.HTTP.Types.Header
import Numeric (showHex)
import System.Random
import System.Random.XRayCustom

-- | Variable for "X-Amzn-Trace-Id" so you don't have to worry about
-- misspelling it.
amazonTraceIdHeaderName :: HeaderName
amazonTraceIdHeaderName = "X-Amzn-Trace-Id"

-- | A trace_id consists of three numbers separated by hyphens. For example,
-- 1-58406520-a006649127e371903a2de979. This includes: The version number, that
-- is, 1. The time of the original request, in Unix epoch time, in 8
-- hexadecimal digits. For example, 10:00AM December 2nd, 2016 PST in epoch
-- time is 1480615200 seconds, or 58406520 in hexadecimal. A 96-bit identifier
-- for the trace, globally unique, in 24 hexadecimal digits.
newtype XRayTraceId = XRayTraceId { unXRayTraceId :: Text }
  deriving (Show, Eq)
  deriving newtype (FromJSON, ToJSON, NFData)

-- TODO: Make a parse function and don't export constructor
-- parseXRayTraceId :: (MonadError String m) => ByteString -> m XRayTraceId

-- | Generates an 'XRayTraceId' in 'IO'. WARNING: This uses the global
-- 'StdGen', so this can be a bottleneck in multi-threaded applications.
generateXRayTraceId :: IORef StdGen -> IO XRayTraceId
generateXRayTraceId ioRef = do
  timeInSeconds <- round <$> getPOSIXTime
  withRandomGenIORef ioRef $ makeXRayTraceId timeInSeconds

makeXRayTraceId :: Int -> StdGen -> (XRayTraceId, StdGen)
makeXRayTraceId timeInSeconds gen = first make $ randomHexString 24 gen
 where
  make hexString =
    XRayTraceId $ T.pack $ "1-" ++ showHex timeInSeconds "" ++ "-" ++ hexString

-- | Generates a random hexadecimal string of a given length.
randomHexString :: Int -> StdGen -> (String, StdGen)
randomHexString n gen =
  replicateRandom n gen $ first intToDigit . randomR (0, 15)

-- | A 64-bit identifier for the segment, unique among segments in the same
-- trace, in 16 hexadecimal digits.
newtype XRaySegmentId = XRaySegmentId { unXRaySegmentId :: Text }
  deriving (Show, Eq)
  deriving newtype (FromJSON, ToJSON, NFData)

-- TODO: Make parse function for XRaySegmentId so it is safer.

-- | Generates an 'XRaySegmentId' using a given 'StdGen'.
generateXRaySegmentId :: StdGen -> (XRaySegmentId, StdGen)
generateXRaySegmentId = first (XRaySegmentId . T.pack) . randomHexString 16

-- | This holds the data from the @X-Amzn-Trace-Id@ header
--
-- See <http://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader>
--
data XRayTraceIdHeaderData = XRayTraceIdHeaderData
  { xrayTraceIdHeaderDataRootTraceId :: !XRayTraceId
  , xrayTraceIdHeaderDataParentId :: !(Maybe XRaySegmentId)
  , xrayTraceIdHeaderDataSampled :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

-- | Constructor for 'XRayTraceIdHeaderData'.
xrayTraceIdHeaderData :: XRayTraceId -> XRayTraceIdHeaderData
xrayTraceIdHeaderData traceId = XRayTraceIdHeaderData
  { xrayTraceIdHeaderDataRootTraceId = traceId
  , xrayTraceIdHeaderDataParentId = Nothing
  , xrayTraceIdHeaderDataSampled = Nothing
  }

-- | Try to parse the value of the X-Amzn-Trace-Id into a
-- 'XRayTraceIdHeaderData'.
parseXRayTraceIdHeaderData :: ByteString -> Maybe XRayTraceIdHeaderData
parseXRayTraceIdHeaderData rawHeader = do
  components <- traverse parseHeaderComponent $ BS8.split ';' rawHeader
  traceId <- lookup "Root" components
  pure XRayTraceIdHeaderData
    { xrayTraceIdHeaderDataRootTraceId = XRayTraceId (T.decodeUtf8 traceId)
    , xrayTraceIdHeaderDataParentId =
      XRaySegmentId . T.decodeUtf8 <$> lookup "Parent" components
    , xrayTraceIdHeaderDataSampled = lookup "Sampled" components >>= readSampled
    }
 where
  readSampled :: ByteString -> Maybe Bool
  readSampled "0" = Just False
  readSampled "1" = Just True
  readSampled _ = Nothing

-- | Turns a 'XRayTraceIdHeaderData' into a 'ByteString' meant for the
-- X-Amzn-Trace-Id header value.
makeXRayTraceIdHeaderValue :: XRayTraceIdHeaderData -> ByteString
makeXRayTraceIdHeaderValue XRayTraceIdHeaderData {..} =
  traceIdPart <> parentPart <> sampledPart
 where
  traceIdPart =
    "Root=" <> T.encodeUtf8 (unXRayTraceId xrayTraceIdHeaderDataRootTraceId)
  parentPart = maybe
    ""
    ((";Parent=" <>) . T.encodeUtf8 . unXRaySegmentId)
    xrayTraceIdHeaderDataParentId
  sampledPart = maybe
    ""
    ((";Sampled=" <>) . BS8.pack . show . fromEnum)
    xrayTraceIdHeaderDataSampled

-- | Header components look like Name=Value
parseHeaderComponent :: ByteString -> Maybe (ByteString, ByteString)
parseHeaderComponent rawComponent = case BS8.split '=' rawComponent of
  [name, value] -> Just (name, value)
  _ -> Nothing
