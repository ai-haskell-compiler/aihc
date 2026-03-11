{-# LANGUAGE QuasiQuotes #-}

module System.CloudFoundry.EnvironmentSpec where

import qualified Data.Map.Strict as Map
import System.Environment (setEnv, unsetEnv)

import Test.Hspec
import Text.RawString.QQ

import qualified System.CloudFoundry.Environment as CfEnv

spec :: Spec
spec = do
  describe "isRunningOnCf" $ do
    it "returns true if VCAP_APPLICATION has a value" $ do
      setEnv "VCAP_APPLICATION" "{}"
      CfEnv.isRunningOnCf `shouldReturn` True

    it "returns true if VCAP_APPLICATION is not set" $ do
      unsetEnv "VCAP_APPLICATION"
      CfEnv.isRunningOnCf `shouldReturn` False

    it "returns true if VCAP_APPLICATION is empty" $ do
      setEnv "VCAP_APPLICATION" ""
      CfEnv.isRunningOnCf `shouldReturn` False

    it "returns true if VCAP_APPLICATION is only whitespace" $ do
      setEnv "VCAP_APPLICATION" "    "
      CfEnv.isRunningOnCf `shouldReturn` False

  describe "current" $
    before setEnvVars $ do
      it "returns error if HOME is not set" $ do
        unsetEnv "HOME"
        CfEnv.current `shouldThrow` anyIOException

      it "returns error if MEMORY_LIMIT is not set" $ do
        unsetEnv "MEMORY_LIMIT"
        CfEnv.current `shouldThrow` anyIOException

      it "returns error if PWD is not set" $ do
        unsetEnv "PWD"
        CfEnv.current `shouldThrow` anyIOException

      it "returns error if PORT is not set" $ do
        unsetEnv "PORT"
        CfEnv.current `shouldThrow` anyIOException

      it "returns error if PORT is not valid number" $ do
        setEnv "PORT" "abc"
        CfEnv.current `shouldThrow` (== CfEnv.NotInteger "PORT" "abc")

      it "returns error if TMPDIR is not set" $ do
        unsetEnv "TMPDIR"
        CfEnv.current `shouldThrow` anyIOException

      it "returns error if USER is not set" $ do
        unsetEnv "USER"
        CfEnv.current `shouldThrow` anyIOException

      it "returns error if VCAP_APPLICATION is not set" $ do
        unsetEnv "VCAP_APPLICATION"
        CfEnv.current `shouldThrow` anyIOException

      it "returns error if VCAP_APPLICATION bad JSON" $ do
        setEnv "VCAP_APPLICATION" "not-json"
        CfEnv.current `shouldThrow` (== CfEnv.DecodeError "VCAP_APPLICATION" "Error in $: string")

      it "returns Application if VCAP_SERVICES is not set" $ do
        unsetEnv "VCAP_SERVICES"
        app <- CfEnv.current
        app `shouldSatisfy` isApplication

      it "returns error if VCAP_SERVICES bad JSON" $ do
        setEnv "VCAP_SERVICES" "not-json"
        CfEnv.current `shouldThrow` (== CfEnv.DecodeError "VCAP_SERVICES" "Error in $: string")

      it "returns Application when the environment is correct" $ do
        let servicesInJson =
              CfEnv.Services $
                  Map.singleton
                    "cleardb"
                    [ CfEnv.Service
                          { CfEnv.name = "service_name"
                          , CfEnv.label = "service_label"
                          , CfEnv.tags = ["tag_a"]
                          , CfEnv.plan = "service_plan"
                          , CfEnv.credentials =
                              Map.fromList
                                [ ("username", "service_username")
                                , ("password", "service_password")
                                ]
                          }
                      ]

        let expected = CfEnv.Application
                { CfEnv.appId = "abc_application_id"
                , CfEnv.applicationUris =
                    ["haskell-test.cfapps.io"]
                , CfEnv.cfApi = "https://api.sys.foundation"
                , CfEnv.home = "/home/userZ"
                , CfEnv.host = "app_host"
                , CfEnv.instanceId = "abc_instance_id"
                , CfEnv.index = 100
                , CfEnv.memoryLimit = "256M"
                , CfEnv.appName = "app_name"
                , CfEnv.pwd = "/pwd"
                , CfEnv.port = 9000
                , CfEnv.tmpDir = "/tmpdir"
                , CfEnv.services = servicesInJson
                , CfEnv.spaceId = "abc_space_id"
                , CfEnv.spaceName = "development"
                , CfEnv.user = "tom"
                , CfEnv.version = "xxx_version"
                , CfEnv.limits =
                    CfEnv.Limits
                      { CfEnv.disk = 1024
                      , CfEnv.fds = 16384
                      , CfEnv.mem = 2048
                      }
                }

        CfEnv.current `shouldReturn` expected

  describe "lookupCurrent" $ do
    it "returns Just Application if running on Cloud Foundry" $ do
      expected <- CfEnv.current
      CfEnv.lookupCurrent `shouldReturn` Just expected

    it "returns Nothing if not running on Cloud Foundry" $ do
      unsetEnv "VCAP_APPLICATION"
      CfEnv.lookupCurrent `shouldReturn` Nothing

  describe "credentialString" $ do
    let service = CfEnv.Service
          { CfEnv.name = "service_name"
          , CfEnv.label = "service_label"
          , CfEnv.tags = ["tag_a"]
          , CfEnv.plan = "service_plan"
          , CfEnv.credentials = Map.singleton "the-key" "the-value"
          }

    it "returns nothing if there is no matching string" $ do
      CfEnv.credentialString "unknown" service `shouldBe` Nothing

    it "returns the string if it is found" $ do
      CfEnv.credentialString "the-key" service `shouldBe` Just "the-value"

  context "for Service searching functions" $ do
    let serviceA =
          CfEnv.Service
            { CfEnv.name = "name-a"
            , CfEnv.label = "label-a"
            , CfEnv.tags = ["good_tag", "ignore_tag"]
            , CfEnv.plan = "plan-a"
            , CfEnv.credentials = Map.empty
            }
    let serviceB =
          CfEnv.Service
            { CfEnv.name = "name-b"
            , CfEnv.label = "label-b"
            , CfEnv.tags = ["ignore_tag"]
            , CfEnv.plan = "plan-b"
            , CfEnv.credentials = Map.empty
            }
    let serviceC =
          CfEnv.Service
            { CfEnv.name = "name-c"
            , CfEnv.label = "label-c"
            , CfEnv.tags = ["good_tag"]
            , CfEnv.plan = "plan-c"
            , CfEnv.credentials = Map.empty
            }
    let services = CfEnv.Services $ Map.fromList
                                      [ ("relational-db", [serviceA, serviceB])
                                      , ("message-queue" , [serviceC])
                                      ]

    describe "withTag" $ do
      it "returns an empty list if no matching services are found" $ do
        CfEnv.withTag "bad_tag" services `shouldBe` []

      it "returns the services with matching tags" $ do
        CfEnv.withTag "good_tag" services `shouldBe` [serviceC , serviceA]

    describe "withName" $ do
      it "returns nothing if there is no service with the given name" $ do
        CfEnv.withName "unknown" services `shouldBe` Nothing

      it "returns the service if it exists" $ do
        CfEnv.withName "name-b" services `shouldBe` Just serviceB

    describe "withLabel" $ do
      it "returns an empty list if there is no service with the given label" $ do
        CfEnv.withLabel "unknown" services `shouldBe` []

      it "returns the services with that labe" $ do
        CfEnv.withLabel "relational-db" services `shouldBe` [serviceA, serviceB]

setEnvVars :: IO ()
setEnvVars = do
  setEnv "HOME" "/home/userZ"
  setEnv "MEMORY_LIMIT" "256M"
  setEnv "PORT" "9000"
  setEnv "PWD" "/pwd"
  setEnv "TMPDIR" "/tmpdir"
  setEnv "USER" "tom"
  setEnv "VCAP_APPLICATION" vcapApplicationJson
  setEnv "VCAP_SERVICES" vcapServicesJson

vcapApplicationJson :: String
vcapApplicationJson =
  [r|{
    "instance_id": "abc_instance_id",
    "application_id": "abc_application_id",
    "application_uris": [
      "haskell-test.cfapps.io"
    ],
    "instance_index": 100,
    "name": "app_name",
    "port": 1080,
    "host": "app_host",
    "version": "xxx_version",
    "application_uris": ["appname.apps.foundation"],
    "space_id": "abc_space_id",
    "space_name": "development",
    "cf_api": "https://api.sys.foundation",
    "limits": {
      "disk": 1024,
      "fds": 16384,
      "mem": 2048
    }
  }|]

vcapServicesJson :: String
vcapServicesJson =
  [r|
    {
      "cleardb": [{
        "name": "service_name",
        "label": "service_label",
        "tags": ["tag_a"],
        "plan": "service_plan",
        "credentials": {
          "username": "service_username",
          "password": "service_password"
        }
      }]
    }
  |]

isApplication :: CfEnv.Application -> Bool
isApplication _ = True