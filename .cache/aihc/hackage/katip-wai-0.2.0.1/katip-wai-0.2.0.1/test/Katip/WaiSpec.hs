module Katip.WaiSpec (spec) where

import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.UUID as Uuid
import qualified Katip
import Katip.Wai.DebugApplication (withDebugApplication)
import qualified Katip.Wai.DebugApplication as DebugApplication
import Katip.Wai.LogEntry (LogEntry (LogEntry))
import qualified Katip.Wai.LogEntry as LogEntry
import qualified Network.HTTP.Client as Http
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldSatisfy)


allSeverities :: [Katip.Severity]
allSeverities = [minBound ..]


spec :: Spec
spec = describe "middleware" $ do
  manager <- runIO $ Http.newManager Http.defaultManagerSettings

  for_ allSeverities $ \severity -> do
    let prettySeverity = Text.unpack . Text.toLower $ Katip.renderSeverity severity

    it ("logs as " <> prettySeverity <> " and adds the request information to the context") $ do
      let message = "Context should be added when severity is " <> Text.pack prettySeverity

      logs <- withDebugApplication severity $ \debugApp ->
        DebugApplication.sendLogRequest manager debugApp "GET" message

      [log1, log2, log3] <- traverse LogEntry.toLogEntry logs

      let expectedRequest =
            LogEntry.Request
              { id = Uuid.nil
              , httpVersion = "HTTP/1.1"
              , remoteHost = "127.0.0.1:1234"
              , isSecure = False
              , method = "GET"
              , path = "/log"
              , queryString = [("message", Just message)]
              , headers = Map.empty
              , receivedAt = "dont care"
              }

      let expectedLog1 =
            LogEntry
              { logMessage = "Request received."
              , logData =
                  LogEntry.LogData
                    { response = Nothing
                    , request = Just expectedRequest
                    }
              , logSeverity = severity
              }
          expectedLog2 =
            LogEntry
              { logMessage = message
              , logData =
                  LogEntry.LogData
                    { response = Nothing
                    , request = Just expectedRequest
                    }
              , logSeverity = severity
              }
          expectedLog3 =
            LogEntry
              { logMessage = "Response sent."
              , logData =
                  LogEntry.LogData
                    { response =
                        Just
                          LogEntry.Response
                            { status = LogEntry.Status 202 "Accepted"
                            , headers = Map.empty
                            , respondedAt = "dont care"
                            , responseTime =
                                LogEntry.ResponseTime
                                  { unit = "dont care"
                                  , time = 0
                                  }
                            }
                    , LogEntry.request = Just expectedRequest
                    }
              , logSeverity = severity
              }

      log1 `LogEntry.shouldBe` expectedLog1
      log2 `LogEntry.shouldBe` expectedLog2
      log3 `LogEntry.shouldBe` expectedLog3

    it ("logs as " <> prettySeverity <> " and uses a different requestId for every request") $ do
      logs <- withDebugApplication severity $ \debugApp ->
        for_ ["GET", "POST", "PATCH", "PUT", "DELETE"] $ \method -> do
          DebugApplication.sendNoContentRequest manager debugApp method
          DebugApplication.sendNotFoundRequest manager debugApp method
          DebugApplication.sendLogRequest manager debugApp method "Some example log message"

      parsedLogs <- traverse LogEntry.toLogEntry logs

      let requestIds =
            flip Maybe.mapMaybe parsedLogs $ \entry -> do
              _response <- LogEntry.response $ LogEntry.logData entry
              request <- LogEntry.request $ LogEntry.logData entry
              pure $ LogEntry.id request

          originalLength = length requestIds

          uniqueLength = Set.size $ Set.fromList requestIds

      originalLength `shouldBe` 15

      uniqueLength `shouldBe` originalLength

    it ("logs as " <> prettySeverity <> " and has non zero elapsed time") $ do
      logs <- withDebugApplication severity $ \debugApp ->
        for_ ["GET", "POST", "PATCH", "PUT", "DELETE"] $ \method -> do
          DebugApplication.sendNoContentRequest manager debugApp method
          DebugApplication.sendNotFoundRequest manager debugApp method
          DebugApplication.sendLogRequest manager debugApp method "Some example log message"

      parsedLogs <- traverse LogEntry.toLogEntry logs

      let elapsedTimes =
            flip Maybe.mapMaybe parsedLogs $ \entry -> do
              response <- LogEntry.response $ LogEntry.logData entry
              pure . LogEntry.time $ LogEntry.responseTime response

      length elapsedTimes `shouldBe` 15

      elapsedTimes `shouldSatisfy` all (> 0)
