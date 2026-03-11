module Amazon.SNS.Verify.ValidateSpec
  ( spec
  ) where

import Amazon.SNS.Verify.TestPrelude

import Amazon.SNS.Verify.Payload
import Amazon.SNS.Verify.Validate
import Data.X509.Validation (SignatureFailure (..))

spec :: Spec
spec = around_ useCertServer $ do
  describe "validateSnsMessage" $ do
    it "successfully validates an SNS notification" $ do
      let message = "Some message"
      x <-
        validateSnsMessage
          $ SNSPayload
            { snsMessage = message
            , snsMessageId = "78d4d7a0-a3eb-5c4d-834f-8d5fa9813ab6"
            , snsTimestamp = "2022-05-18T14:52:26.952Z"
            , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
            , snsType = "Notification"
            , snsSignatureVersion = "1"
            , snsSignature =
                "Dg24trcOUiLjclt5JwyJS0JEOnEEbi6P30XS6KBxMCwzZ08a04UwjaFTW9Ae8xurhBS5YESz1fY28vTwvEmxh/20WmB3bWIDOMp9v5RI8XSZOvpMm+hdQJ43VqGhEDyAvRU6iCDLihDlZNc/sBCwl9X0H4kh/8vIElRif9gFBbYI94ZHGgqEV+Zc3gVKo9Udrl/MxNvMVadsO/+/oPVUeWibQr3xfGK95oc/ocuNAgi0MOxZmLVnibHu36KOTSvy2qSLonnRRFcbaauYZJ4js7oTq+1ujXNO72oPLaeG3pVJ2grqMc5z8tKQxFnSTE3es7wQarU/CLrbO8j0isbnWw=="
            , snsSigningCertURL = cert
            , snsTypePayload =
                Notification
                  $ SNSNotification
                    { snsSubject =
                        Just
                          "SynthesisTaskNotification { TaskId: 680a1f1b-f3ae-4474-aa8f-3b6dfe52e656, Status: COMPLETED }"
                    }
            }
      x `shouldBe` Right (SNSMessage message)

    it "fails to validate a currupt SNS notification" $ do
      let go =
            validateSnsMessage
              $ SNSPayload
                { snsMessage = "Some message"
                , snsMessageId = "corrupt"
                , snsTimestamp = "2022-05-18T14:52:26.952Z"
                , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
                , snsType = "Notification"
                , snsSignatureVersion = "1"
                , snsSignature =
                    "Dg24trcOUiLjclt5JwyJS0JEOnEEbi6P30XS6KBxMCwzZ08a04UwjaFTW9Ae8xurhBS5YESz1fY28vTwvEmxh/20WmB3bWIDOMp9v5RI8XSZOvpMm+hdQJ43VqGhEDyAvRU6iCDLihDlZNc/sBCwl9X0H4kh/8vIElRif9gFBbYI94ZHGgqEV+Zc3gVKo9Udrl/MxNvMVadsO/+/oPVUeWibQr3xfGK95oc/ocuNAgi0MOxZmLVnibHu36KOTSvy2qSLonnRRFcbaauYZJ4js7oTq+1ujXNO72oPLaeG3pVJ2grqMc5z8tKQxFnSTE3es7wQarU/CLrbO8j0isbnWw=="
                , snsSigningCertURL = cert
                , snsTypePayload =
                    Notification
                      $ SNSNotification
                        { snsSubject =
                            Just
                              "SynthesisTaskNotification { TaskId: 680a1f1b-f3ae-4474-aa8f-3b6dfe52e656, Status: COMPLETED }"
                        }
                }
      go `shouldReturn` Left (InvalidPayload SignatureInvalid)

    it "fails to validate a bad PEM" $ do
      let go =
            validateSnsMessage
              $ SNSPayload
                { snsMessage = "Some message"
                , snsMessageId = "corrupt"
                , snsTimestamp = "2022-05-18T14:52:26.952Z"
                , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
                , snsType = "Notification"
                , snsSignatureVersion = "1"
                , snsSignature =
                    "Dg24trcOUiLjclt5JwyJS0JEOnEEbi6P30XS6KBxMCwzZ08a04UwjaFTW9Ae8xurhBS5YESz1fY28vTwvEmxh/20WmB3bWIDOMp9v5RI8XSZOvpMm+hdQJ43VqGhEDyAvRU6iCDLihDlZNc/sBCwl9X0H4kh/8vIElRif9gFBbYI94ZHGgqEV+Zc3gVKo9Udrl/MxNvMVadsO/+/oPVUeWibQr3xfGK95oc/ocuNAgi0MOxZmLVnibHu36KOTSvy2qSLonnRRFcbaauYZJ4js7oTq+1ujXNO72oPLaeG3pVJ2grqMc5z8tKQxFnSTE3es7wQarU/CLrbO8j0isbnWw=="
                , snsSigningCertURL = "http://localhost:3000/404"
                , snsTypePayload =
                    Notification
                      $ SNSNotification
                        { snsSubject =
                            Just
                              "SynthesisTaskNotification { TaskId: 680a1f1b-f3ae-4474-aa8f-3b6dfe52e656, Status: COMPLETED }"
                        }
                }
      go `shouldReturn` Left (BadPem "Empty List")

    it "fails to validate an unexpected url" $ do
      let go =
            validateSnsMessage
              $ SNSPayload
                { snsMessage = "Some message"
                , snsMessageId = "corrupt"
                , snsTimestamp = "2022-05-18T14:52:26.952Z"
                , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
                , snsType = "Notification"
                , snsSignatureVersion = "1"
                , snsSignature =
                    "Dg24trcOUiLjclt5JwyJS0JEOnEEbi6P30XS6KBxMCwzZ08a04UwjaFTW9Ae8xurhBS5YESz1fY28vTwvEmxh/20WmB3bWIDOMp9v5RI8XSZOvpMm+hdQJ43VqGhEDyAvRU6iCDLihDlZNc/sBCwl9X0H4kh/8vIElRif9gFBbYI94ZHGgqEV+Zc3gVKo9Udrl/MxNvMVadsO/+/oPVUeWibQr3xfGK95oc/ocuNAgi0MOxZmLVnibHu36KOTSvy2qSLonnRRFcbaauYZJ4js7oTq+1ujXNO72oPLaeG3pVJ2grqMc5z8tKQxFnSTE3es7wQarU/CLrbO8j0isbnWw=="
                , snsSigningCertURL = "http://attacker.com/evil.pem"
                , snsTypePayload =
                    Notification
                      $ SNSNotification
                        { snsSubject =
                            Just
                              "SynthesisTaskNotification { TaskId: 680a1f1b-f3ae-4474-aa8f-3b6dfe52e656, Status: COMPLETED }"
                        }
                }
      go `shouldReturn` Left (BadUri "http://attacker.com/evil.pem")

    it "successfully validates an SNS subscription" $ do
      -- pendingWith "need valid subscription payload"
      let
        message =
          "You have chosen to subscribe to the topic arn:aws:sns:us-west-2:123456789012:MyTopic.\nTo confirm the subscription, visit the SubscribeURL included in this message."
        subscription =
          SNSSubscription
            { snsToken = "2336412f37..."
            , snsSubscribeURL =
                "https://sns.us-west-2.amazonaws.com/?Action=ConfirmSubscription&TopicArn=arn:aws:sns:us-west-2:123456789012:MyTopic&Token=2336412f37..."
            }
      x <-
        validateSnsMessage
          $ SNSPayload
            { snsMessage = message
            , snsMessageId = "165545c9-2a5c-472c-8df2-7ff2be2b3b1b"
            , snsTimestamp = "2012-04-26T20:45:04.751Z"
            , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
            , snsType = "SubscriptionConfirmation"
            , snsSignatureVersion = "1"
            , snsSignature =
                "Jec/VlsopbiA2fCckj/IwTPjDbSuFkl2hNKL898sQuZcMeKeOLthYs7YlF+xLi+Ip6rG/X08GZKtCqpoiSgKW8D9PI6eHVM2JQa76sFJO5ZdPylrDH+URwBf28gT+1l/VYk4p3VK8RZo+3Wkn87HXwxTq1YoN390o5ncT34zaBDtLx2cUA8+JOnYjItmYjVXDhrEBF6xad/vIY8V2o5xyQOfEWLm71/Tcs3radzNoSj2xlLQyJKPOzV661fG6Xz1vVKfDVC03+Q4Pn67SmU1wWRRT1nDwPPzQlcDAiAGRjB1U/C5iHfLQFF3dKo4azylkrM2ReTCMm9KMyIWqjq5eg=="
            , snsSigningCertURL = cert
            , snsTypePayload = SubscriptionConfirmation subscription
            }
      x `shouldBe` Right (SNSSubscribe subscription)

    it "successfully validates an SNS unsubscribe" $ do
      -- pendingWith "need valid subscription payload"
      let
        message =
          "You have chosen to subscribe to the topic arn:aws:sns:us-west-2:123456789012:MyTopic.\nTo confirm the subscription, visit the SubscribeURL included in this message."
        subscription =
          SNSSubscription
            { snsToken = "2336412f37..."
            , snsSubscribeURL =
                "https://sns.us-west-2.amazonaws.com/?Action=ConfirmSubscription&TopicArn=arn:aws:sns:us-west-2:123456789012:MyTopic&Token=2336412f37..."
            }
      x <-
        validateSnsMessage
          $ SNSPayload
            { snsMessage = message
            , snsMessageId = "165545c9-2a5c-472c-8df2-7ff2be2b3b1b"
            , snsTimestamp = "2012-04-26T20:45:04.751Z"
            , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
            , snsType = "UnsubscribeConfirmation"
            , snsSignatureVersion = "1"
            , snsSignature =
                "fKtmZTE6xvGhbcCTchFPLmuhdoXI7hxWrE9qe1RjeLDecMaZGmqsn4rOrFDsteqot4ItLuJqvV7RtImGXrMa/JNnZdP71lG6FdrKTiGqZNrnxZZYbIuZMAsSQM4E8VaRwbxLXuPQY9IYFP4y9GfsdpDYx0tpbXOxGz/JFVQjTFpHY55BmV6Ec73g0X/eLSEdKfHtWg/gVf6W27ewa40jXvaa78VmcVXbPXIKwzGgSSSe9t6xxVe0kLjKXaDyJTl3rbZJZJgBLInbychWNq1vGHGZQhtCyxjKRfKIWNWDbHdM/fUBGUhuv089CblWpq8g/21HiJ9n+S3VSn0hCXB5hg=="
            , snsSigningCertURL = cert
            , snsTypePayload = UnsubscribeConfirmation subscription
            }
      x `shouldBe` Right (SNSUnsubscribe subscription)

cert :: Text
cert = "http://localhost:3000"
