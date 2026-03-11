module Katip.Wai.LogEntry
  ( Request (..)
  , Status (..)
  , ResponseTime (..)
  , Response (..)
  , LogData (..)
  , LogEntry (..)
  , requestHeaders
  , responseHeaders
  , toLogEntry
  , shouldBe
  )
where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import qualified Katip
import qualified Test.Hspec as Hspec


data Request = Request
  { id :: UUID
  , method :: Text
  , httpVersion :: Text
  , path :: Text
  , headers :: Map Text Text
  , isSecure :: Bool
  , remoteHost :: Text
  , queryString :: [(Text, Maybe Text)]
  , receivedAt :: Text
  }
  deriving (Show, Eq, Generic)


instance Aeson.FromJSON Request


requestHeaders :: Request -> Map Text Text
requestHeaders Request{headers} =
  headers


data ResponseTime = ResponseTime
  { unit :: Text
  , time :: Double
  }
  deriving (Show, Eq, Generic)


instance Aeson.FromJSON ResponseTime


data Status = Status
  { code :: Natural
  , message :: Text
  }
  deriving (Show, Eq, Generic)


instance Aeson.FromJSON Status


data Response = Response
  { status :: Status
  , headers :: Map Text Text
  , respondedAt :: Text
  , responseTime :: ResponseTime
  }
  deriving (Show, Eq, Generic)


responseHeaders :: Response -> Map Text Text
responseHeaders Response{headers} =
  headers


instance Aeson.FromJSON Response


data LogData = LogData
  { request :: Maybe Request
  , response :: Maybe Response
  }
  deriving (Generic, Show)


instance Aeson.FromJSON LogData


data LogEntry = LogEntry
  { logMessage :: Text
  , logData :: LogData
  , logSeverity :: Katip.Severity
  }
  deriving (Show)


instance Aeson.FromJSON LogEntry where
  parseJSON = Aeson.withObject "LogEntry" $ \obj ->
    LogEntry <$> (obj .: "msg") <*> (obj .: "data") <*> (obj .: "sev")


toLogEntry :: Aeson.Value -> IO LogEntry
toLogEntry value =
  case Aeson.fromJSON value of
    Aeson.Success entry -> pure entry
    Aeson.Error reason -> fail reason


shouldBe :: HasCallStack => LogEntry -> LogEntry -> IO ()
shouldBe log1 log2 = do
  on Hspec.shouldBe logMessage log1 log2

  on Hspec.shouldBe (fmap method . request . logData) log1 log2
  on Hspec.shouldBe (fmap httpVersion . request . logData) log1 log2
  on Hspec.shouldBe (fmap path . request . logData) log1 log2
  on Hspec.shouldBe (fmap isSecure . request . logData) log1 log2
  on Hspec.shouldBe (fmap queryString . request . logData) log1 log2

  on Hspec.shouldBe (fmap status . response . logData) log1 log2

  on Hspec.shouldBe logSeverity log1 log2
