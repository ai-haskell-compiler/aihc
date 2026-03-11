module Plaid.PlaidSpec (spec) where

import qualified Control.Error as ER
import           Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode')
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Either (isRight)
import           Data.Plaid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (fromGregorian)
import           Test.Hspec

_runTestGetRequest :: PlaidHttp m => Text -> m ByteString
_runTestGetRequest url = executeGet url

runTestPostRequest :: (PlaidHttp m, ToJSON a) => Text -> a -> m ByteString
runTestPostRequest url postBody = executePost url postBody

checkResult :: FromJSON a => ByteString -> Either PlaidError a
checkResult result =
  case result of
    "" -> Left (PlaidError $ T.pack $ "Failed to decode bytestring " ++ (unpack $ toStrict result))
    _  ->
      case eitherDecode' result of
        Left e   -> Left $ PlaidError (T.pack e)
        Right ok -> Right ok

env :: PlaidEnv
env = PlaidEnv "" (ClientId "") (Secret "") Sandbox

publicTokenBody :: Either PlaidError (PlaidBody PublicTokenCreate)
publicTokenBody =
  mkCreatePublicTokenEnv env
    (InstitutionId "ins_3")
    ["transactions"]
    Nothing
    -- (Just $
    --   PlaidOptions
    --   "http://bogicevicsasa.com"
    --   "user_good"
    --   "pass_good"
    -- )

spec :: Spec
spec = do
  describe "Create public token" $ do
    it "/public_token/create" $ do
      let result = runTestPlaid $ runTestPostRequest "/public_token/create" publicTokenBody
          testResult = checkResult result :: Either PlaidError PlaidPublicTokenResponse
      isRight testResult `shouldBe` True

  describe "Exchange public token" $ do
    it "/item/public_token/exchange" $ do
      let result = runTestPlaid $ runTestPostRequest "/public_token/create" publicTokenBody
      testResult <- ER.runExceptT $ do
        PlaidPublicTokenResponse {..} <- ER.hoistEither (checkResult result :: Either PlaidError PlaidPublicTokenResponse)
        exchangeBody <- ER.hoistEither (mkExchangePublicTokenEnv env _plaidPublicTokenResponsePublicToken)
        let result' = runTestPlaid $ runTestPostRequest "/item/public_token/exchange" exchangeBody
        ER.hoistEither (checkResult result' :: Either PlaidError PlaidAccessTokenResponse)
      isRight testResult `shouldBe` True

  describe "Auth get" $ do
    it "/auth/get" $ do
      let result = runTestPlaid $ runTestPostRequest "/public_token/create" publicTokenBody
      testResult <- ER.runExceptT $ do
        PlaidPublicTokenResponse {..} <- ER.hoistEither (checkResult result :: Either PlaidError PlaidPublicTokenResponse)
        exchangeBody <- ER.hoistEither (mkExchangePublicTokenEnv env _plaidPublicTokenResponsePublicToken)
        let result' = runTestPlaid $ runTestPostRequest "/item/public_token/exchange" exchangeBody
        PlaidAccessTokenResponse {..} <- ER.hoistEither (checkResult result' :: Either PlaidError PlaidAccessTokenResponse)
        authBody <- ER.hoistEither (mkGetAuthEnv env _plaidAccessTokenResponseAccessToken)
        let authGetResult = runTestPlaid $ runTestPostRequest "/auth/get" authBody
        ER.hoistEither (checkResult authGetResult :: Either PlaidError PlaidAuthGetResponse)
      isRight testResult `shouldBe` True

  describe "Identity get" $ do
    it "/identity/get" $ do
      let result = runTestPlaid $ runTestPostRequest "/public_token/create" publicTokenBody
      testResult <- ER.runExceptT $ do
        PlaidPublicTokenResponse {..} <- ER.hoistEither (checkResult result :: Either PlaidError PlaidPublicTokenResponse)
        exchangeBody <- ER.hoistEither (mkExchangePublicTokenEnv env _plaidPublicTokenResponsePublicToken)
        let result' = runTestPlaid $ runTestPostRequest "/item/public_token/exchange" exchangeBody
        PlaidAccessTokenResponse {..} <- ER.hoistEither (checkResult result' :: Either PlaidError PlaidAccessTokenResponse)
        identityBody <- ER.hoistEither (mkCreateIdentityGetEnv env _plaidAccessTokenResponseAccessToken)
        let identityGetResult = runTestPlaid $ runTestPostRequest "/identity/get" identityBody
        ER.hoistEither (checkResult identityGetResult :: Either PlaidError PlaidIdentityGetResponse)
      isRight testResult `shouldBe` True
  describe "Transactions get" $ do
    it "/transactions/get" $ do
      let result = runTestPlaid $ runTestPostRequest "/public_token/create" publicTokenBody
      testResult <- ER.runExceptT $ do
        PlaidPublicTokenResponse {..} <- ER.hoistEither (checkResult result :: Either PlaidError PlaidPublicTokenResponse)
        exchangeBody <- ER.hoistEither (mkExchangePublicTokenEnv env _plaidPublicTokenResponsePublicToken)
        let result' = runTestPlaid $ runTestPostRequest "/item/public_token/exchange" exchangeBody
        PlaidAccessTokenResponse {..} <- ER.hoistEither (checkResult result' :: Either PlaidError PlaidAccessTokenResponse)
        authBody <- ER.hoistEither (mkGetAuthEnv env _plaidAccessTokenResponseAccessToken)
        let authGetResult = runTestPlaid $ runTestPostRequest "/auth/get" authBody
        PlaidAuthGetResponse {..} <- ER.hoistEither (checkResult authGetResult :: Either PlaidError PlaidAuthGetResponse)
        let startTime = fromGregorian 2018 1 1
        let endTime   = fromGregorian 2019 1 1
        transactionBody <-
          ER.hoistEither (mkCreateTransactionsGetEnv
                           env
                           _plaidAccessTokenResponseAccessToken
                           startTime
                           endTime
                           Nothing
                           Nothing
                         )

        let transactionsGetResult =
              runTestPlaid $ runTestPostRequest "/transactions/get" transactionBody
        ER.hoistEither (checkResult transactionsGetResult :: Either PlaidError PlaidTransactionsGetResponse)
      isRight testResult `shouldBe` True
  describe "Balance json" $ do
    it "should be able to decode Balance" $ do
      let result = eitherDecode' balanceJson :: Either String Balance
      isRight result `shouldBe` True
  describe "Account json" $ do
    it "should be able to decode Account" $ do
      let result = eitherDecode' accountJson :: Either String Account
      isRight result `shouldBe` True
  describe "Account list json" $ do
    it "should be able to decode Account list" $ do
      let result = eitherDecode' accountListJson :: Either String [Account]
      isRight result `shouldBe` True
  describe "PaymentMeta json" $ do
    it "should be able to decode PaymentMeta" $ do
      let result = eitherDecode' accountJson :: Either String PaymentMeta
      isRight result `shouldBe` True
  describe "Item json" $ do
    it "should be able to decode Item" $ do
      let result = eitherDecode' itemJson :: Either String Item
      isRight result `shouldBe` True
  describe "Transaction json" $ do
    it "should be able to decode Transaction" $ do
      let result = eitherDecode' transactionJson :: Either String Transaction
      isRight result `shouldBe` True
  describe "TransactionLocation json" $ do
    it "should be able to decode TransactionLocation" $ do
      let result = eitherDecode' transactionLocationJson :: Either String TransactionLocation
      isRight result `shouldBe` True
  describe "email json" $ do
    it "should be able to decode Email" $ do
      let result = eitherDecode' emailJson :: Either String Email
      isRight result `shouldBe` True
  describe "phone json" $ do
    it "should be able to decode Phone" $ do
      let result = eitherDecode' phoneNumberJson :: Either String PhoneNumber
      isRight result `shouldBe` True
  describe "address json" $ do
    it "should be able to decode Addresses" $ do
      let result = eitherDecode' addressesJson :: Either String Addresses
      isRight result `shouldBe` True
  describe "Accounts json" $ do
    it "should be able to decode Accounts" $ do
      let result = eitherDecode' accountsJson :: Either String Accounts
      isRight result `shouldBe` True
  describe "Owners json" $ do
    it "should be able to decode Owners" $ do
      let result = eitherDecode' ownersJson :: Either String Owners
      isRight result `shouldBe` True
