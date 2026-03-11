module Amazon.SNS.Verify.Payload
  ( SNSPayload (..)
  , SNSType (..)
  , SNSNotification (..)
  , SNSSubscription (..)
  ) where

import Amazon.SNS.Verify.Prelude

import Data.Aeson
  ( FromJSON
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , parseJSON
  , withObject
  , (.:)
  )
import GHC.Generics (Generic)

data SNSPayload = SNSPayload
  { snsMessage :: Text
  , snsMessageId :: Text
  , snsTimestamp :: Text
  , snsTopicArn :: Text
  , snsType :: Text
  , snsSignatureVersion :: Text
  , snsSignature :: Text
  , snsSigningCertURL :: Text
  , snsTypePayload :: SNSType
  }

instance FromJSON SNSPayload where
  parseJSON v = parse v
   where
    parse = withObject "SNSPayload" $ \o -> do
      payloadType <- o .: "Type"
      SNSPayload
        <$> o
        .: "Message"
        <*> o
        .: "MessageId"
        <*> o
        .: "Timestamp"
        <*> o
        .: "TopicArn"
        <*> pure payloadType
        <*> o
        .: "SignatureVersion"
        <*> o
        .: "Signature"
        <*> o
        .: "SigningCertURL"
        <*> parseType payloadType
    parseType = \case
      "SubscriptionConfirmation" -> SubscriptionConfirmation <$> parseJSON v
      "UnsubscribeConfirmation" -> UnsubscribeConfirmation <$> parseJSON v
      "Notification" -> Notification <$> parseJSON v
      msg -> fail $ "Unknown message type " <> show msg

data SNSType
  = Notification SNSNotification
  | SubscriptionConfirmation SNSSubscription
  | UnsubscribeConfirmation SNSSubscription

newtype SNSNotification = SNSNotification
  { snsSubject :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SNSNotification where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = drop 3}

data SNSSubscription = SNSSubscription
  { snsToken :: Text
  , snsSubscribeURL :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SNSSubscription where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = drop 3}
