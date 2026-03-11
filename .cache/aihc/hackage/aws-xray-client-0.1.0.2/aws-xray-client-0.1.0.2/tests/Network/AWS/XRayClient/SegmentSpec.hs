{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.AWS.XRayClient.SegmentSpec
  ( spec
  ) where

import Prelude

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.QQ
import Test.Hspec

import Network.AWS.XRayClient.Segment
import Network.AWS.XRayClient.TraceId

spec :: Spec
spec = do
  let
    traceId = XRayTraceId "1-5880168b-fd5158284b67678a3bb5a78c"
    segmentId = XRaySegmentId "6b55dcc497934f1a"
    startTime = 123.456
    endTime = 789.123

  describe "Segment JSON instances" $ do
    it "should correctly encode the minimum segment" $ do
      let
        segment =
          xraySegment
          "my-service"
          segmentId
          traceId
          startTime
          (Just endTime)
      toJSON segment `shouldBe`
        [aesonQQ|
          {
            "id": #{segmentId},
            "trace_id": #{traceId},
            "name": "my-service",
            "start_time": #{startTime},
            "end_time": #{endTime}
          }
        |]

    it "should handle in_progress properly" $ do
      let
        segment =
          xraySegment
          "my-service"
          segmentId
          traceId
          startTime
          Nothing
      toJSON segment `shouldBe`
        [aesonQQ|
          {
            "id": #{segmentId},
            "trace_id": #{traceId},
            "name": "my-service",
            "start_time": #{startTime},
            "in_progress": true
          }
        |]

    -- NB: This is basically a big integration test taken from the AWS docs
    it "should handle a super long segment" $ do
      toJSON bigSegment `shouldBe` bigSegmentJSON

bigSegment :: XRaySegment
bigSegment =
  xraySegment
    "www.example.com"
    (XRaySegmentId "6b55dcc497934f1a")
    (XRayTraceId "1-5880168b-fd5158284b67678a3bb5a78c")
    1484789387.126
    (Just 1484789387.535)
  & xraySegmentOrigin ?~ "AWS::EC2::Instance"
  & xraySegmentHttp ?~ (
    xraySegmentHttpDef
    & xraySegmentHttpRequest ?~ (
      xraySegmentHttpRequestDef
      & xraySegmentHttpRequestMethod ?~ "POST"
      & xraySegmentHttpRequestClientIp ?~ "78.255.233.48"
      & xraySegmentHttpRequestUrl ?~ "http://www.example.com/api/user"
      & xraySegmentHttpRequestUserAgent ?~ "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:45.0) Gecko/20100101 Firefox/45.0"
      & xraySegmentHttpRequestXForwardedFor ?~ True
    )
    & xraySegmentHttpResponse ?~ (
      xraySegmentHttpResponseDef
      & xraySegmentHttpResponseStatus ?~ 200
    )
  )
  & xraySegmentAnnotations ?~
    [ "customer_category" .= (124 :: Int)
    , "zip_code" .= (98101 :: Int)
    , "country" .= ("United States" :: String)
    , "internal" .= False
    ]
  & xraySegmentAws ?~ (
    xraySegmentAwsDef
    & xraySegmentAwsEc2 ?~ (
      xraySegmentAwsEc2Def
      & xraySegmentAwsEc2InstanceId ?~ "i-0b5a4678fc325bg98"
      & xraySegmentAwsEc2AvailabilityZone ?~ "us-west-2c"
    )
  )
  & xraySegmentSubsegments ?~
    [ xraySubsegment
        "RDS"
        (XRaySegmentId "0f910026178b71eb")
        1484789387.502
        (Just 1484789387.534)
      & xraySegmentNamespace ?~ "aws"
      & xraySegmentHttp ?~ (
        xraySegmentHttpDef
        & xraySegmentHttpResponse ?~ (
          xraySegmentHttpResponseDef
          & xraySegmentHttpResponseStatus ?~ 200
          & xraySegmentHttpResponseContentLength ?~ 58
        )
      )
      & xraySegmentSql ?~ (
        xraySegmentSqlDef
        & xraySegmentSqlUrl ?~ "jdbc:postgresql://aawijb5u25wdoy.cpamxznpdoq8.us-west-2.rds.amazonaws.com:5432/ebdb"
        & xraySegmentSqlPreparation ?~ "statement"
        & xraySegmentSqlDatabaseType ?~ "PostgreSQL"
        & xraySegmentSqlDatabaseVersion ?~ "9.5.4"
        & xraySegmentSqlDriverVersion ?~ "PostgreSQL 9.4.1211.jre7"
        & xraySegmentSqlUser ?~ "dbuser"
        & xraySegmentSqlSanitizedQuery ?~ "SELECT  *  FROM  customers  WHERE  customer_id=?;"
      )
    ]

bigSegmentJSON :: Value
bigSegmentJSON =
  [aesonQQ|
{
  "id": "6b55dcc497934f1a",
  "start_time": 1484789387.126,
  "end_time": 1484789387.535,
  "trace_id": "1-5880168b-fd5158284b67678a3bb5a78c",
  "name": "www.example.com",
  "origin": "AWS::EC2::Instance",
  "aws": {
    "ec2": {
      "availability_zone": "us-west-2c",
      "instance_id": "i-0b5a4678fc325bg98"
    }
  },
  "annotations": {
    "customer_category" : 124,
    "zip_code" : 98101,
    "country" : "United States",
    "internal" : false
  },
  "http": {
    "request": {
      "method": "POST",
      "client_ip": "78.255.233.48",
      "url": "http://www.example.com/api/user",
      "user_agent": "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:45.0) Gecko/20100101 Firefox/45.0",
      "x_forwarded_for": true
    },
    "response": {
      "status": 200
    }
  },
  "subsegments": [
    {
      "id": "0f910026178b71eb",
      "start_time": 1484789387.502,
      "end_time": 1484789387.534,
      "name": "RDS",
      "namespace": "aws",
      "http": {
        "response": {
          "content_length": 58,
          "status": 200
        }
      },
      "sql" : {
        "url": "jdbc:postgresql://aawijb5u25wdoy.cpamxznpdoq8.us-west-2.rds.amazonaws.com:5432/ebdb",
        "preparation": "statement",
        "database_type": "PostgreSQL",
        "database_version": "9.5.4",
        "driver_version": "PostgreSQL 9.4.1211.jre7",
        "user" : "dbuser",
        "sanitized_query" : "SELECT  *  FROM  customers  WHERE  customer_id=?;"
      }
    }
  ]
}
|]
