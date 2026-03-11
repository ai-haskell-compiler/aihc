{-# LANGUAGE QuasiQuotes #-}

module Amazon.SNS.VerifySpec
  ( spec
  ) where

import Amazon.SNS.Verify.TestPrelude

import Amazon.SNS.Verify
import Data.Aeson.QQ

spec :: Spec
spec = around_ useCertServer $ do
  describe "verifySNSMessage" $ do
    it "successfully validates an SNS notification" $ do
      let payload =
            [aesonQQ|
          { Message: "Some message"
          , MessageId: "78d4d7a0-a3eb-5c4d-834f-8d5fa9813ab6"
          , Timestamp: "2022-05-18T14:52:26.952Z"
          , TopicArn: "arn:aws:sns:us-west-2:123456789012:MyTopic"
          , Type: "Notification"
          , Subject: "SynthesisTaskNotification { TaskId: 680a1f1b-f3ae-4474-aa8f-3b6dfe52e656, Status: COMPLETED }"
          , SignatureVersion: "1"
          , Signature: "Dg24trcOUiLjclt5JwyJS0JEOnEEbi6P30XS6KBxMCwzZ08a04UwjaFTW9Ae8xurhBS5YESz1fY28vTwvEmxh/20WmB3bWIDOMp9v5RI8XSZOvpMm+hdQJ43VqGhEDyAvRU6iCDLihDlZNc/sBCwl9X0H4kh/8vIElRif9gFBbYI94ZHGgqEV+Zc3gVKo9Udrl/MxNvMVadsO/+/oPVUeWibQr3xfGK95oc/ocuNAgi0MOxZmLVnibHu36KOTSvy2qSLonnRRFcbaauYZJ4js7oTq+1ujXNO72oPLaeG3pVJ2grqMc5z8tKQxFnSTE3es7wQarU/CLrbO8j0isbnWw=="
          , SigningCertURL: "http://localhost:3000"
          }
          |]
      x <- verifySNSMessage payload

      x `shouldBe` "Some message"

    it "successfully confirms a subscription" $ do
      let payload =
            [aesonQQ|
          { Message: "Some message"
          , MessageId: "78d4d7a0-a3eb-5c4d-834f-8d5fa9813ab6"
          , Timestamp: "2022-05-18T14:52:26.952Z"
          , Token: "test"
          , TopicArn: "arn:aws:sns:us-west-2:123456789012:MyTopic"
          , Type: "SubscriptionConfirmation"
          , SignatureVersion: "1"
          , SubscribeURL: "http://localhost:3000"

          , Signature: "qTSmo1uWEdGbXRUizLdSHA9AejQjOKKUBpqKc7tNdCNRHbFzlLE4ILq7rSY6HnepRWh6KMMjxXTEYz0hgKqb7XRz5xPZQW35MaMdg3pOy1J3ZH3o0pdareaCIwBIMKRX4Fg0+MTgg9aYrM/j6Rt9pjLpdl6rYaYLF/hv0uTnMJ7SbhrFz3nJGzpHI9p2qGfp9G+Fd5UG6aXUiok34LjlE1Kq8LLJSglV9+hW2ZKSkYwVVrqixlaZamHyjGXrtCqPO+TKWjxp0G8SCf0zHe+CNTq0D4ZeDNKixh1GHXh23kc85gnH8YnfsBeuHudXK8Fhum9v/9Rr5YGLQNf5+3qmtA=="
          , SigningCertURL: "http://localhost:3000"
          }
          |]

      verifySNSMessage payload
        `shouldThrow` ( \case
                          SubscribeMessageResponded -> True
                          _ -> False
                      )
