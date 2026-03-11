module Main where

{-
 Module containing useful real world examples of using plaid library.
-}

import           Control.Exception.Safe (MonadThrow, throwM)
import           Control.Monad.Except   (liftEither, runExceptT)
import           Control.Monad.Reader
import           Data.ByteString.Lazy   (ByteString)
import           Data.Plaid
import           Data.Text              (Text)
import           Data.Time
import           Text.Pretty.Simple

-- | Client id
clientId :: ClientId
clientId = ClientId "5d11dd0fdeb8a80013cc99e9"

-- | Public key
publicKey :: PublicKey
publicKey = "f19293a734d8e9cc89faacb09719e5"

-- | Sandbox secret key
sandboxSecret :: Secret
sandboxSecret = Secret "0d5e0ec25d432388f7fb34d3528394"

-- | Public token
publicToken :: PublicToken
publicToken = PublicToken "public-sandbox-e5c3a0d9-822b-4d8c-bd13-4253485c99b2"

-- | Access token
accessToken :: AccessToken
accessToken =  AccessToken "access-sandbox-9e96926d-4da6-4620-a13b-c72980976436"

-- | Example of item id
itemId :: Text
itemId = "Qlr98nQgkdTQyM8agmjGt3a4RV6lKxup88BLV"

-- | Main function from which we call different api endpoints for demonstration
main :: IO ()
main = do
  let env = PlaidEnv publicKey clientId sandboxSecret Sandbox
  exchangeTokenResponse <- runPlaid env exampleExchangeToken
  pPrint exchangeTokenResponse
  authResponse <- runPlaid env exampleRetrieveAuthRequest
  pPrint authResponse
  transactionsResponse <- runPlaid env exampleRetrieveTransactionsRequest
  pPrint transactionsResponse
  identityResponse <- runPlaid env exampleIdentityRequest
  pPrint identityResponse
  incomeResponse <- runPlaid env exampleIncomeRequest
  pPrint incomeResponse

-- | POST /item/public_token/exchange
--  https://plaid.com/docs/#exchange-token-flow
exampleExchangeToken
  :: ( MonadReader PlaidEnv m
     , MonadIO m
     , MonadThrow m
     , PlaidHttp m
     )
  => m (Either PlaidError PlaidAccessTokenResponse)
exampleExchangeToken = do
  -- get the plaid environment
  env <- ask
  runExceptT $ do
    -- create the public token request body
    -- this action can fail if we provide wrong parameters
    publicTokenBody <- liftEither $
        mkCreatePublicTokenEnv env
          -- Pay attention to provide valid institution id! There is a list of valid ones
          -- in the docs https:://plaid.com/docs/#investments-top-institutions-we-cover
          (InstitutionId "ins_3")
          -- Provide the list of plaid products or empty list
          ["transactions"]
          -- This is optional. If you want you can setup a web-hook where plaid will send its responses.
          defaultPlaidOptions
    -- fire the create public token action
    PlaidPublicTokenResponse {..} <- lift $ plaidCreatePublicToken publicTokenBody
    -- use the response to create an exchange public token request body
    -- it looks something like this:
    -- @
    -- PlaidBody {plaidBodyEnv = PlaidEnv {plaidEnvPublicKey = "f19293a734d8e9cc89faacb09719e5", plaidEnvClientId = ClientId {unClientId = "5d11dd0fdeb8a80013cc99e9"}, plaidEnvSecret = Secret {unSecret = "0d5e0ec25d432388f7fb34d3528394"}, plaidEnvEnvironment = sandbox}, plaidBodyPublicToken = Just (PublicToken {unPublicToken = "public-sandbox-6a6d0da0-4d53-4702-a1eb-9b430625122b"}), plaidBodyAccessToken = Nothing, plaidBodyOptions = Nothing, plaidBodyInstitutionId = Nothing, plaidBodyInitialProducts = Nothing}
    -- @

    exchangeBody <- liftEither (mkExchangePublicTokenEnv env _plaidPublicTokenResponsePublicToken)
    -- fire exchange token action
    lift $ plaidExchangeToken exchangeBody
    -- if the response is successfull it looks something like this:
    -- @
    -- Right (PlaidAccessTokenResponse {plaidAccessTokenResponseAccessToken = "access-sandbox-0ca6b500-c682-437e-bdc9-051665b71941", plaidAccessTokenResponseRequestId = "2vBC1rv95r7Se60", plaidAccessTokenResponseItemId = "mdwNyLvv9zF655lGQ1bxikExVGb4doULb5VPp"})
    -- @
    
getAuthBody
  :: ( MonadReader PlaidEnv m
     , MonadIO m
     , MonadThrow m
     , PlaidHttp m
     )
  => m (Either PlaidError (PlaidBody AuthGet))
getAuthBody = do
  env <- ask
  runExceptT $ do
    publicTokenBody <- liftEither $ createPublicTokenBody env
    PlaidAccessTokenResponse {..} <- lift $ exchangeToken publicTokenBody
    liftEither (mkGetAuthEnv env _plaidAccessTokenResponseAccessToken)

-- | POST /auth/get
-- https://plaid.com/docs/#retrieve-auth-request
exampleRetrieveAuthRequest
  :: ( MonadReader PlaidEnv m
     , MonadIO m
     , MonadThrow m
     , PlaidHttp m
     )
  => m (Either PlaidError ByteString)
exampleRetrieveAuthRequest =
  -- get the plaid environment
  runExceptT $ do
    -- create the public token request body
    -- this action can fail if we provide wrong parameters
    eauthBody <- lift getAuthBody
    authBody <- liftEither eauthBody
    lift $ plaidGetAuth authBody

-- | POST /transactions/get
-- https://plaid.com/docs/#retrieve-transactions-request
exampleRetrieveTransactionsRequest
  :: ( MonadReader PlaidEnv m
     , MonadIO m
     , MonadThrow m
     , PlaidHttp m
     )
  => m (Either PlaidError ByteString)
exampleRetrieveTransactionsRequest = do
  -- get the plaid environment
  env <- ask
  runExceptT $ do
    -- create the public token request body
    -- this action can fail if we provide wrong parameters
    publicTokenBody <- liftEither $ createPublicTokenBody env
    PlaidAccessTokenResponse {..} <- lift $ exchangeToken publicTokenBody
    authBody <- liftEither (mkGetAuthEnv env _plaidAccessTokenResponseAccessToken)
    _ <- lift $ plaidGetAuth authBody
    let startTime = fromGregorian 2018 1 1
        endTime   = fromGregorian 2019 1 1
    transactionBody <-
      liftEither
        (mkCreateTransactionsGetEnv
           env
           _plaidAccessTokenResponseAccessToken
           startTime
           endTime
           Nothing
           (Just defPaginationOptions)
        )
    lift $ plaidGetTransactions transactionBody
-- | POST /income/get
-- http://plaid.com/docs/#income
exampleIncomeRequest
  :: ( MonadReader PlaidEnv m
     , MonadIO m
     , MonadThrow m
     , PlaidHttp m
     )
  => m (Either PlaidError ByteString)
exampleIncomeRequest = do
  env <- ask
  runExceptT $ do
    -- create the public token request body
    -- this action can fail if we provide wrong parameters
    publicTokenBody <- liftEither $ createPublicTokenBody env
    PlaidAccessTokenResponse {..} <- lift $ exchangeToken publicTokenBody
    incomeBody <- liftEither (mkCreateIncomeGetEnv env _plaidAccessTokenResponseAccessToken)
    lift $ plaidGetIncome incomeBody

-- | POST /identity/get
-- http://plaid.com/docs/#identity
exampleIdentityRequest
  :: ( MonadReader PlaidEnv m
     , MonadIO m
     , MonadThrow m
     , PlaidHttp m
     )
  => m (Either PlaidError ByteString)
exampleIdentityRequest = do
  env <- ask
  runExceptT $ do
    -- create the public token request body
    -- this action can fail if we provide wrong parameters
    publicTokenBody <- liftEither $ createPublicTokenBody env
    PlaidAccessTokenResponse {..} <- lift $ exchangeToken publicTokenBody
    identityBody <- liftEither (mkCreateIdentityGetEnv env _plaidAccessTokenResponseAccessToken)
    lift $ plaidGetIdentity identityBody

exchangeToken
  :: ( MonadReader PlaidEnv m
     , MonadIO m
     , MonadThrow m
     , PlaidHttp m
     )
  => PlaidBody PublicTokenCreate
  -> m PlaidAccessTokenResponse
exchangeToken publicTokenBody = do
  env <- ask
  -- fire the create public token action
  PlaidPublicTokenResponse {..} <- plaidCreatePublicToken publicTokenBody
  case mkExchangePublicTokenEnv env _plaidPublicTokenResponsePublicToken of
    Left err           -> throwM err
    Right exchangeBody -> plaidExchangeToken exchangeBody

createPublicTokenBody :: PlaidEnv -> Either PlaidError (PlaidBody PublicTokenCreate)
createPublicTokenBody env =
  mkCreatePublicTokenEnv env
    -- pay attention to provide valid institution id! There is a list of valid ones
    -- in the docs https:://plaid.com/docs/#investments-top-institutions-we-cover
    (InstitutionId "ins_3")
    -- provide the list of plaid products or empty list
    ["transactions"]
    -- this is optional. If you want you can setup a web-hook where plaid will send its responses.
    mempty

defaultPlaidOptions :: Maybe PlaidOptions
defaultPlaidOptions =
  Just $
     PlaidOptions
       { _plaidOptionsWebHook = "http://bogicevicsasa.com"
       , _plaidOptionsOverrideUsername = "user_good"
       , _plaidOptionsOverridePassword = "pass_good"
       }
