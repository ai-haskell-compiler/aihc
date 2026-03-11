{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.XRayClient.Segment
  ( -- * Segment
    XRaySegment(..)
  , xraySegment
  , xraySubsegment
  , xrayIndependentSubsegment

  , xraySegmentName
  , xraySegmentId
  , xraySegmentTraceId
  , xraySegmentStartTime
  , xraySegmentEndTime
  , xraySegmentInProgress
  , xraySegmentParentId
  , xraySegmentType
  , xraySegmentOrigin
  , xraySegmentUser
  , xraySegmentNamespace
  , xraySegmentService
  , xraySegmentError
  , xraySegmentThrottle
  , xraySegmentFault
  , xraySegmentCause
  , xraySegmentHttp
  , xraySegmentAnnotations
  , xraySegmentMetadata
  , xraySegmentSubsegments
  , xraySegmentAws
  , xraySegmentSql

  , XRaySegmentService(..)
  , xraySegmentServiceVersion

    -- * HTTP
  , XRaySegmentHttp(..)
  , xraySegmentHttpDef
  , xraySegmentHttpRequest
  , xraySegmentHttpResponse

  , XRaySegmentHttpRequest(..)
  , xraySegmentHttpRequestDef
  , xraySegmentHttpRequestMethod
  , xraySegmentHttpRequestUrl
  , xraySegmentHttpRequestUserAgent
  , xraySegmentHttpRequestClientIp
  , xraySegmentHttpRequestXForwardedFor
  , xraySegmentHttpRequestTraced

  , XRaySegmentHttpResponse(..)
  , xraySegmentHttpResponseDef
  , xraySegmentHttpResponseStatus
  , xraySegmentHttpResponseContentLength

    -- * AWS Resource Data
  , XRaySegmentAws(..)
  , xraySegmentAwsDef
  , xraySegmentAwsAccountId
  , xraySegmentAwsEcs
  , xraySegmentAwsEc2
  , xraySegmentAwsElasticBeanstalk
  , xraySegmentAwsOperation
  , xraySegmentAwsRegion
  , xraySegmentAwsRequestId
  , xraySegmentAwsQueueUrl
  , xraySegmentAwsTableName

  , XRaySegmentAwsEcs(..)
  , xraySegmentAwsEcsDef
  , xraySegmentAwsEcsContainer

  , XRaySegmentAwsEc2(..)
  , xraySegmentAwsEc2Def
  , xraySegmentAwsEc2InstanceId
  , xraySegmentAwsEc2AvailabilityZone

  , XRaySegmentAwsElasticBeanstalk(..)
  , xraySegmentAwsElasticBeanstalkDef
  , xraySegmentAwsElasticBeanstalkEnvironmentName
  , xraySegmentAwsElasticBeanstalkVersionLabel
  , xraySegmentAwsElasticBeanstalkDeploymentId

    -- * SQL
  , XRaySegmentSql(..)
  , xraySegmentSqlDef
  , xraySegmentSqlConnectionString
  , xraySegmentSqlUrl
  , xraySegmentSqlSanitizedQuery
  , xraySegmentSqlDatabaseType
  , xraySegmentSqlDatabaseVersion
  , xraySegmentSqlDriverVersion
  , xraySegmentSqlUser
  , xraySegmentSqlPreparation
  ) where

import Prelude

import Control.Lens.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Network.AWS.XRayClient.JSONHelpers
import Network.AWS.XRayClient.TraceId

-- TODO: Make different types for segment, subsegment, and independent
-- subsegment instead of relying on different smart constructors. The
-- sub-sections, like aws, will also have different variants.

-- | Represents an entire X-Ray Segment document. See
-- <http://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-fields>
data XRaySegment
  = XRaySegment
  { _xraySegmentName :: !Text
    -- ^ The logical name of the service that handled the request, up to 200
    -- characters. For example, your application's name or domain name. Names
    -- can contain Unicode letters, numbers, and whitespace, and the following
    -- symbols: _, ., :, /, %, &, #, =, +, \, -, @
  , _xraySegmentId :: !XRaySegmentId
    -- ^ A 64-bit identifier for the segment, unique among segments in the same
    -- trace, in 16 hexadecimal digits.
  , _xraySegmentTraceId :: !(Maybe XRayTraceId)
    -- ^ A unique identifier that connects all segments and subsegments
    -- originating from a single client request.
  , _xraySegmentStartTime :: !POSIXTime
    -- ^ number that is the time the segment was created, in floating point
    -- seconds in epoch time. For example, 1480615200.010 or 1.480615200010E9.
    -- Use as many decimal places as you need. Microsecond resolution is
    -- recommended when available.
  , _xraySegmentEndTime :: !(Maybe POSIXTime)
    -- ^ number that is the time the segment was closed. For example,
    -- 1480615200.090 or 1.480615200090E9. Specify either an end_time or
    -- in_progress.
  , _xraySegmentInProgress :: !(Maybe Bool)
    -- ^ boolean, set to true instead of specifying an end_time to record that
    -- a segment is started, but is not complete. Send an in-progress segment
    -- when your application receives a request that will take a long time to
    -- serve, to trace the request receipt. When the response is sent, send the
    -- complete segment to overwrite the in-progress segment. Only send one
    -- complete segment, and one or zero in-progress segments, per request.
  , _xraySegmentParentId :: !(Maybe XRaySegmentId)
    -- ^ A subsegment ID you specify if the request originated from an
    -- instrumented application. The X-Ray SDK adds the parent subsegment ID to
    -- the tracing header for downstream HTTP calls.
  , _xraySegmentType :: !(Maybe Text)
    -- ^ Set this to @"subsegment"@ if this is an independent subsegment.
  , _xraySegmentOrigin :: !(Maybe Text)
    -- ^ The type of AWS resource running your application.
  , _xraySegmentUser :: !(Maybe Text)
    -- ^ A string that identifies the user who sent the request.
  , _xraySegmentNamespace :: !(Maybe Text)
    -- ^ "aws" for AWS SDK calls; "remote" for other downstream calls.
  , _xraySegmentService :: !(Maybe XRaySegmentService)
    -- ^ An object with information about your application.
  , _xraySegmentError :: !(Maybe Bool)
    -- ^ fields that indicate an error occurred and that include information
    -- about the exception that caused the error.
  , _xraySegmentThrottle :: !(Maybe Bool)
    -- ^ fields that indicate an error occurred and that include information
    -- about the exception that caused the error.
  , _xraySegmentFault :: !(Maybe Bool)
    -- ^ fields that indicate an error occurred and that include information
    -- about the exception that caused the error.

    -- TODO: Make cause more type-safe
  , _xraySegmentCause :: !(Maybe Value)
    -- ^ fields that indicate an error occurred and that include information
    -- about the exception that caused the error.
  , _xraySegmentHttp :: !(Maybe XRaySegmentHttp)
    -- ^ 'XRaySegmentHttp' object with information about the original HTTP
    -- request.
  , _xraySegmentAnnotations :: !(Maybe Object)
    -- ^ object with key-value pairs that you want X-Ray to index for search.
  , _xraySegmentMetadata :: !(Maybe Object)
    -- ^ object with any additional data that you want to store in the segment.
  , _xraySegmentSubsegments :: !(Maybe [XRaySegment])
    -- ^ array of 'XRaySegment' objects. See 'xraySubSegment' for a smart
    -- constructor.
  , _xraySegmentAws :: !(Maybe XRaySegmentAws)
    -- ^ object with information about the AWS resource on which your
    -- application served the request.
  , _xraySegmentSql :: !(Maybe XRaySegmentSql)
    -- ^ Object representing a sql query.
  } deriving (Show, Eq)

-- | Smart constructor for 'XRaySegment' with all the required fields.
--
-- Note that in 'XRaySegment', @end_time@ and @in_progress@ are mutually
-- exclusive. If your @end_time@ is 'Nothing', then @in_progress@ will be set
-- to 'True'.
xraySegment
  :: Text -- ^ name
  -> XRaySegmentId -- ^ id
  -> XRayTraceId -- ^ trace_id
  -> POSIXTime -- ^ start_time
  -> Maybe POSIXTime -- ^ end_time
  -> XRaySegment
xraySegment name segmentId traceId startTime mEndTime =
  (xraySubsegment name segmentId startTime mEndTime)
    { _xraySegmentTraceId = Just traceId
    }

-- | An 'XRaySegment' meant to be used as an embedded subsegment in another
-- 'XRaySegment'.
xraySubsegment
  :: Text -- ^ name
  -> XRaySegmentId -- ^ id
  -> POSIXTime -- ^ start_time
  -> Maybe POSIXTime -- ^ end_time
  -> XRaySegment
xraySubsegment name segmentId startTime mEndTime = XRaySegment
  { _xraySegmentName = name
  , _xraySegmentId = segmentId
  , _xraySegmentTraceId = Nothing
  , _xraySegmentStartTime = startTime
    -- NB: We must either specify end time or in_progress = true
  , _xraySegmentEndTime = mEndTime
  , _xraySegmentInProgress = if isNothing mEndTime then Just True else Nothing
  , _xraySegmentParentId = Nothing
  , _xraySegmentType = Nothing
  , _xraySegmentOrigin = Nothing
  , _xraySegmentUser = Nothing
  , _xraySegmentNamespace = Nothing
  , _xraySegmentService = Nothing
  , _xraySegmentError = Nothing
  , _xraySegmentThrottle = Nothing
  , _xraySegmentFault = Nothing
  , _xraySegmentCause = Nothing
  , _xraySegmentHttp = Nothing
  , _xraySegmentAnnotations = Nothing
  , _xraySegmentMetadata = Nothing
  , _xraySegmentSubsegments = Nothing
  , _xraySegmentAws = Nothing
  , _xraySegmentSql = Nothing
  }

-- | Smart constructor for an independent subsegment. Includes additional
-- required fields.
xrayIndependentSubsegment
  :: Text -- ^ name
  -> XRaySegmentId -- ^ id
  -> XRayTraceId -- ^ trace_id
  -> POSIXTime -- ^ start_time
  -> Maybe POSIXTime -- ^ end_time
  -> XRaySegmentId -- ^ parent_id
  -> XRaySegment
xrayIndependentSubsegment name segmentId traceId startTime mEndTime parentId =
  (xraySegment name segmentId traceId startTime mEndTime)
    { _xraySegmentType = Just "subsegment"
    , _xraySegmentParentId = Just parentId
    }

-- | Type for the @service@ field of a segment document.
newtype XRaySegmentService
  = XRaySegmentService
  { _xraySegmentServiceVersion :: Text
    -- ^ A string that identifies the version of your application that served
    -- the request.
  } deriving (Show, Eq)

-- | See
-- <http://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-http>
data XRaySegmentHttp
  = XRaySegmentHttp
  { _xraySegmentHttpRequest :: !(Maybe XRaySegmentHttpRequest)
  , _xraySegmentHttpResponse :: !(Maybe XRaySegmentHttpResponse)
  } deriving (Show, Eq)

-- | Constructor for 'XRaySegmentHttp' with fields initialized to 'Nothing'.
xraySegmentHttpDef :: XRaySegmentHttp
xraySegmentHttpDef = XRaySegmentHttp
  { _xraySegmentHttpRequest = Nothing
  , _xraySegmentHttpResponse = Nothing
  }

data XRaySegmentHttpRequest
  = XRaySegmentHttpRequest
  { _xraySegmentHttpRequestMethod :: !(Maybe Text)
    -- ^ The request method. For example, GET.
  , _xraySegmentHttpRequestUrl :: !(Maybe Text)
    -- ^ The full URL of the request, compiled from the protocol, hostname, and
    -- path of the request.
  , _xraySegmentHttpRequestUserAgent :: !(Maybe Text)
    -- ^ The user agent string from the requester's client.
  , _xraySegmentHttpRequestClientIp :: !(Maybe Text)
    -- ^ The IP address of the requester. Can be retrieved from the IP packet's
    -- Source Address or, for forwarded requests, from an X-Forwarded-For
    -- header.
  , _xraySegmentHttpRequestXForwardedFor :: !(Maybe Bool)
    -- ^ (segments only) boolean indicating that the client_ip was read from an
    -- X-Forwarded-For header and is not reliable as it could have been forged.
  , _xraySegmentHttpRequestTraced :: !(Maybe Bool)
    -- ^ (subsegments only) boolean indicating that the downstream call is to
    -- another traced service. If this field is set to true, X-Ray considers
    -- the trace to be broken until the downstream service uploads a segment
    -- with a parent_id that matches the id of the subsegment that contains
    -- this block.
  } deriving (Show, Eq)

-- | Constructor for 'XRaySegmentHttpRequest' with fields initialized to 'Nothing'.
xraySegmentHttpRequestDef :: XRaySegmentHttpRequest
xraySegmentHttpRequestDef = XRaySegmentHttpRequest
  { _xraySegmentHttpRequestMethod = Nothing
  , _xraySegmentHttpRequestUrl = Nothing
  , _xraySegmentHttpRequestUserAgent = Nothing
  , _xraySegmentHttpRequestClientIp = Nothing
  , _xraySegmentHttpRequestXForwardedFor = Nothing
  , _xraySegmentHttpRequestTraced = Nothing
  }

data XRaySegmentHttpResponse
  = XRaySegmentHttpResponse
  { _xraySegmentHttpResponseStatus :: !(Maybe Int)
    -- ^ number indicating the HTTP status of the response.
  , _xraySegmentHttpResponseContentLength :: !(Maybe Int)
    -- ^ number indicating the length of the response body in bytes.
  } deriving (Show, Eq)

-- | Constructor for 'XRaySegmentHttpResponse' with fields initialized to 'Nothing'.
xraySegmentHttpResponseDef :: XRaySegmentHttpResponse
xraySegmentHttpResponseDef = XRaySegmentHttpResponse
  { _xraySegmentHttpResponseStatus = Nothing
  , _xraySegmentHttpResponseContentLength = Nothing
  }

-- | Type for the @aws@ field in a segment. See
-- <http://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-aws>
--
-- NOTE: Please see the documentation for what fields are allowed in a segment
-- versus a subsegment.
data XRaySegmentAws
  = XRaySegmentAws
  { _xraySegmentAwsAccountId :: !(Maybe Text)
    -- ^ If your application sends segments to a different AWS account, record
    -- the ID of the account running your application.
  , _xraySegmentAwsEcs :: !(Maybe XRaySegmentAwsEcs)
    -- ^ Information about an Amazon ECS container.
  , _xraySegmentAwsEc2 :: !(Maybe XRaySegmentAwsEc2)
    -- ^ Information about an EC2 instance.
  , _xraySegmentAwsElasticBeanstalk :: !(Maybe XRaySegmentAwsElasticBeanstalk)
    -- ^ Information about an Elastic Beanstalk environment. You can find this
    -- information in a file named /var/elasticbeanstalk/xray/environment.conf
    -- on the latest Elastic Beanstalk platforms.
  , _xraySegmentAwsOperation :: !(Maybe Text)
    -- ^ The name of the API action invoked against an AWS service or resource.
  , _xraySegmentAwsRegion :: !(Maybe Text)
    -- ^ If the resource is in a region different from your application, record
    -- the region. For example, us-west-2.
  , _xraySegmentAwsRequestId :: !(Maybe Text)
    -- ^ Unique identifier for the request.
  , _xraySegmentAwsQueueUrl :: !(Maybe Text)
    -- ^ For operations on an Amazon SQS queue, the queue's URL.
  , _xraySegmentAwsTableName :: !(Maybe Text)
    -- ^ For operations on a DynamoDB table, the name of the table.
  } deriving (Show, Eq)

-- | Constructor for 'XRaySegmentAws' with fields initialized to 'Nothing'.
xraySegmentAwsDef :: XRaySegmentAws
xraySegmentAwsDef = XRaySegmentAws
  { _xraySegmentAwsAccountId = Nothing
  , _xraySegmentAwsEcs = Nothing
  , _xraySegmentAwsEc2 = Nothing
  , _xraySegmentAwsElasticBeanstalk = Nothing
  , _xraySegmentAwsOperation = Nothing
  , _xraySegmentAwsRegion = Nothing
  , _xraySegmentAwsRequestId = Nothing
  , _xraySegmentAwsQueueUrl = Nothing
  , _xraySegmentAwsTableName = Nothing
  }

newtype XRaySegmentAwsEcs
  = XRaySegmentAwsEcs
  { _xraySegmentAwsEcsContainer :: Maybe Text
    -- ^ The container ID of the container running your application.
  } deriving (Show, Eq)

-- | Constructor for 'XRaySegmentAwsEcs' with fields initialized to 'Nothing'.
xraySegmentAwsEcsDef :: XRaySegmentAwsEcs
xraySegmentAwsEcsDef =
  XRaySegmentAwsEcs {_xraySegmentAwsEcsContainer = Nothing}

data XRaySegmentAwsEc2
  = XRaySegmentAwsEc2
  { _xraySegmentAwsEc2InstanceId :: !(Maybe Text)
    -- ^ The instance ID of the EC2 instance.
  , _xraySegmentAwsEc2AvailabilityZone :: !(Maybe Text)
    -- ^ The Availability Zone in which the instance is running.
  } deriving (Show, Eq)

-- | Constructor for 'XRaySegmentAwsEc2' with fields initialized to 'Nothing'.
xraySegmentAwsEc2Def :: XRaySegmentAwsEc2
xraySegmentAwsEc2Def = XRaySegmentAwsEc2
  { _xraySegmentAwsEc2InstanceId = Nothing
  , _xraySegmentAwsEc2AvailabilityZone = Nothing
  }

data XRaySegmentAwsElasticBeanstalk
  = XRaySegmentAwsElasticBeanstalk
  { _xraySegmentAwsElasticBeanstalkEnvironmentName :: !(Maybe Text)
    -- ^ The name of the environment.
  , _xraySegmentAwsElasticBeanstalkVersionLabel :: !(Maybe Text)
    -- ^ The name of the application version that is currently deployed to the
    -- instance that served the request.
  , _xraySegmentAwsElasticBeanstalkDeploymentId :: !(Maybe Int)
    -- ^ number indicating the ID of the last successful deployment to the
    -- instance that served the request.
  } deriving (Show, Eq)

-- | Constructor for 'XRaySegmentAwsElasticBeanstalk' with fields initialized
-- to 'Nothing'.
xraySegmentAwsElasticBeanstalkDef :: XRaySegmentAwsElasticBeanstalk
xraySegmentAwsElasticBeanstalkDef = XRaySegmentAwsElasticBeanstalk
  { _xraySegmentAwsElasticBeanstalkEnvironmentName = Nothing
  , _xraySegmentAwsElasticBeanstalkVersionLabel = Nothing
  , _xraySegmentAwsElasticBeanstalkDeploymentId = Nothing
  }

data XRaySegmentSql
  = XRaySegmentSql
  { _xraySegmentSqlConnectionString :: !(Maybe Text)
    -- ^ For SQL Server or other database connections that don't use URL
    -- connection strings, record the connection string, excluding passwords.
  , _xraySegmentSqlUrl :: !(Maybe Text)
    -- ^ For a database connection that uses a URL connection string, record
    -- the URL, excluding passwords.
  , _xraySegmentSqlSanitizedQuery :: !(Maybe Text)
    -- ^ The database query, with any user provided values removed or replaced
    -- by a placeholder.
  , _xraySegmentSqlDatabaseType :: !(Maybe Text)
    -- ^ The name of the database engine.
  , _xraySegmentSqlDatabaseVersion :: !(Maybe Text)
    -- ^ The version number of the database engine.
  , _xraySegmentSqlDriverVersion :: !(Maybe Text)
    -- ^ The name and version number of the database engine driver that your
    -- application uses.
  , _xraySegmentSqlUser :: !(Maybe Text)
    -- ^ The database username.
  , _xraySegmentSqlPreparation :: !(Maybe Text)
    -- ^ call if the query used a PreparedCall; statement if the query used a
    -- PreparedStatement.
  } deriving (Show, Eq)

-- | Constructor for 'XRaySegmentSql' with fields initialized to 'Nothing'.
xraySegmentSqlDef :: XRaySegmentSql
xraySegmentSqlDef = XRaySegmentSql
  { _xraySegmentSqlConnectionString = Nothing
  , _xraySegmentSqlUrl = Nothing
  , _xraySegmentSqlSanitizedQuery = Nothing
  , _xraySegmentSqlDatabaseType = Nothing
  , _xraySegmentSqlDatabaseVersion = Nothing
  , _xraySegmentSqlDriverVersion = Nothing
  , _xraySegmentSqlUser = Nothing
  , _xraySegmentSqlPreparation = Nothing
  }

makeLenses ''XRaySegment
makeLenses ''XRaySegmentHttp
makeLenses ''XRaySegmentHttpRequest
makeLenses ''XRaySegmentHttpResponse
makeLenses ''XRaySegmentAws
makeLenses ''XRaySegmentAwsEcs
makeLenses ''XRaySegmentAwsEc2
makeLenses ''XRaySegmentAwsElasticBeanstalk
makeLenses ''XRaySegmentSql
makeLenses ''XRaySegmentService

deriveJSON (xrayAesonOptions "_xraySegmentService") ''XRaySegmentService
deriveJSON (xrayAesonOptions "_xraySegmentSql") ''XRaySegmentSql
deriveJSON (xrayAesonOptions "_xraySegmentAwsElasticBeanstalk") ''XRaySegmentAwsElasticBeanstalk
deriveJSON (xrayAesonOptions "_xraySegmentAwsEc2") ''XRaySegmentAwsEc2
deriveJSON (xrayAesonOptions "_xraySegmentAwsEcs") ''XRaySegmentAwsEcs
deriveJSON (xrayAesonOptions "_xraySegmentAws") ''XRaySegmentAws
deriveJSON (xrayAesonOptions "_xraySegmentHttpResponse") ''XRaySegmentHttpResponse
deriveJSON (xrayAesonOptions "_xraySegmentHttpRequest") ''XRaySegmentHttpRequest
deriveJSON (xrayAesonOptions "_xraySegmentHttp") ''XRaySegmentHttp
deriveJSON (xrayAesonOptions "_xraySegment") ''XRaySegment
