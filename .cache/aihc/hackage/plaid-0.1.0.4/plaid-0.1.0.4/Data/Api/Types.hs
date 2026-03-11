{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs       #-}

module Data.Api.Types where

import           Control.Exception.Safe
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader (..), ReaderT (..))
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Api.TestByteStrings
import           Data.ByteString.Lazy     (ByteString)
import           Data.Fixed
import           Data.Functor.Identity
import qualified Data.Map                 as M
import           Data.Maybe               (isJust, isNothing)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (Day)
import           Lens.Micro
import           Lens.Micro.TH
import           Network.HTTP.Conduit
import           Text.Casing              (camel, quietSnake)

data Environment
  = Sandbox
  | Development
  | Production deriving (Ord, Eq)

$(deriveFromJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 11 }) ''Environment)

instance Show Environment where
  show Sandbox     = "sandbox"
  show Development = "development"
  show Production  = "production"

class Monad m => PlaidHttp m where
  executePost :: ToJSON a => Text -> a -> m ByteString
  executeGet :: Text -> m ByteString

newtype PlaidError =
  PlaidError Text
  deriving Show
  deriving anyclass Exception

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 10 }) ''PlaidError)

newtype AccessToken =
  AccessToken
    { unAccessToken :: Text
    } deriving (Eq, Show)

instance ToJSON AccessToken where
  toJSON AccessToken {..} = String unAccessToken

instance FromJSON AccessToken where
  parseJSON = withText "AccessToken" $ \t ->
    return $ AccessToken t

newtype PublicToken =
  PublicToken
    { unPublicToken :: Text
    } deriving (Eq, Show)

instance ToJSON PublicToken where
  toJSON PublicToken {..} = String unPublicToken

instance FromJSON PublicToken where
  parseJSON = withText "PublicToken" $ \t ->
    return $ PublicToken t

newtype ClientId =
  ClientId
    { unClientId :: Text
    } deriving (Eq, Show)

instance ToJSON ClientId where
  toJSON ClientId {..} = String unClientId

instance FromJSON ClientId where
  parseJSON = withText "ClientId" $ \t ->
    return $ ClientId t

newtype Secret =
  Secret
    { unSecret :: Text
    } deriving (Eq, Show)

instance ToJSON Secret where
  toJSON Secret {..} = String unSecret

instance FromJSON Secret where
  parseJSON = withText "Secret" $ \t ->
    return $ Secret t

newtype InstitutionId =
  InstitutionId
    { unInstitutionId :: Text
    } deriving (Eq, Show)

instance ToJSON InstitutionId where
  toJSON InstitutionId {..} = String unInstitutionId

instance FromJSON InstitutionId where
  parseJSON = withText "InstitutionId" $ \t ->
    return $ InstitutionId t

newtype Url a = Url { unUrl :: Text }

instance ToJSON (Url a) where
  toJSON Url {..} = String unUrl

instance FromJSON (Url a) where
  parseJSON = withText "Url" $ \t ->
    return $ Url t

type PublicKey     = Text
type AccessKey     = Text
type PlaidProduct  = Text
type Institution = Text
type AccountNumber = Text
type SortCode = Text
type AccountId = Text
type Iban = Text
type Bic = Text
type RequestId = Text

data AuthGet
data GetBalance
data PublicTokenCreate
data PlaidTokenExchange
data PlaidTransactionsGet
data PlaidIdentityGet
data PlaidIncomeGet

data PlaidOptions =
  PlaidOptions
    { _plaidOptionsWebHook          :: Text
    , _plaidOptionsOverrideUsername :: Text
    , _plaidOptionsOverridePassword :: Text
    } deriving (Eq, Show)

makeLenses ''PlaidOptions
$(deriveFromJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 13 }) ''PlaidOptions)

instance ToJSON PlaidOptions where
  toJSON b =
    object [ "webhook"           .= (b ^. plaidOptionsWebHook)
           , "override_username" .= (b ^. plaidOptionsOverrideUsername)
           , "override_password" .= (b ^. plaidOptionsOverridePassword)
           ]

data PlaidPaginationOptions =
  PlaidPaginationOptions
    { _plaidPaginationOptionsCount  :: Int
    , _plaidPaginationOptionsOffset :: Int
    } deriving (Eq, Show)

makeLenses ''PlaidPaginationOptions
$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 23 }) ''PlaidPaginationOptions)

defPaginationOptions :: PlaidPaginationOptions
defPaginationOptions =
  PlaidPaginationOptions
    { _plaidPaginationOptionsCount  = 100
    , _plaidPaginationOptionsOffset = 0
    }

data PlaidEnv =
  PlaidEnv
    { _plaidEnvPublicKey   :: PublicKey
    , _plaidEnvClientId    :: ClientId
    , _plaidEnvSecret      :: Secret
    , _plaidEnvEnvironment :: Environment
    } deriving (Eq, Show)

makeLenses ''PlaidEnv
$(deriveFromJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 8 }) ''PlaidEnv)

data PlaidBody a
  = PlaidBody
      { _plaidBodyEnv               :: PlaidEnv
      , _plaidBodyPublicToken       :: Maybe PublicToken
      , _plaidBodyAccessToken       :: Maybe AccessToken
      , _plaidBodyOptions           :: Maybe PlaidOptions
      , _plaidBodyPaginationOptions :: Maybe PlaidPaginationOptions
      , _plaidBodyInstitutionId     :: Maybe InstitutionId
      , _plaidBodyInitialProducts   :: Maybe [PlaidProduct]
      , _plaidBodyStartDate         :: Maybe Day
      , _plaidBodyEndDate           :: Maybe Day
      } deriving (Eq, Show)

makeLenses ''PlaidBody

instance ToJSON (PlaidBody GetBalance) where
  toJSON b =
    if isNothing (b ^. plaidBodyOptions)
      then
        object [ "client_id"    .= (b ^. plaidBodyEnv . plaidEnvClientId)
               , "secret"       .= (b ^. plaidBodyEnv . plaidEnvSecret)
               , "access_token" .= toJSON (b ^. plaidBodyAccessToken)
               ]
      else
        object [ "client_id"    .= (b ^. plaidBodyEnv . plaidEnvClientId)
               , "secret"       .= (b ^. plaidBodyEnv . plaidEnvSecret)
               , "access_token" .= (b ^. plaidBodyAccessToken)
               , "options"      .= (b ^. plaidBodyOptions)
               ]

instance ToJSON (PlaidBody PublicTokenCreate) where
  toJSON b =
    if isNothing (b ^. plaidBodyOptions)
      then
        object [ "institution_id"   .= (b ^. plaidBodyInstitutionId)
               , "public_key"       .= (b ^. plaidBodyEnv . plaidEnvPublicKey)
               , "initial_products" .= (b ^. plaidBodyInitialProducts)
               ]
      else
        object [ "institution_id"   .= (b ^. plaidBodyInstitutionId)
               , "public_key"       .= (b ^. plaidBodyEnv . plaidEnvPublicKey)
               , "initial_products" .= (b ^. plaidBodyInitialProducts)
               , "options"          .= toJSON (b ^. plaidBodyOptions)
               ]

instance ToJSON (PlaidBody AuthGet) where
  toJSON b =
    object [ "client_id"    .= (b ^. plaidBodyEnv . plaidEnvClientId)
           , "secret"       .= (b ^. plaidBodyEnv . plaidEnvSecret)
           , "access_token" .= (b ^. plaidBodyAccessToken)
           ]

instance ToJSON (PlaidBody PlaidTokenExchange) where
  toJSON b =
    object [ "client_id"    .= (b ^. plaidBodyEnv . plaidEnvClientId)
           , "secret"       .= (b ^. plaidBodyEnv . plaidEnvSecret)
           , "public_token" .= (b ^. plaidBodyPublicToken)
           ]

instance ToJSON (PlaidBody PlaidTransactionsGet) where
  toJSON b =
    object $ [ "client_id"    .= (b ^. plaidBodyEnv . plaidEnvClientId)
             , "secret"       .= (b ^. plaidBodyEnv . plaidEnvSecret)
             , "access_token" .= (b ^. plaidBodyAccessToken)
             , "start_date"   .= (b ^. plaidBodyStartDate)
             , "end_date"     .= (b ^. plaidBodyEndDate)
             ] <> perhapsSendObject
    where
      perhapsSendObject =
        ["options" .= toJSON (b ^. plaidBodyPaginationOptions) | isJust (b ^. plaidBodyPaginationOptions)]

instance ToJSON (PlaidBody PlaidIdentityGet) where
  toJSON b =
    object [ "client_id"    .= (b ^. plaidBodyEnv . plaidEnvClientId)
           , "secret"       .= (b ^. plaidBodyEnv . plaidEnvSecret)
           , "access_token" .= (b ^. plaidBodyAccessToken)
           ]

instance ToJSON (PlaidBody PlaidIncomeGet) where
  toJSON b =
    object [ "client_id"    .= (b ^. plaidBodyEnv . plaidEnvClientId)
           , "secret"       .= (b ^. plaidBodyEnv . plaidEnvSecret)
           , "access_token" .= (b ^. plaidBodyAccessToken)
           ]

-- | Main type used as the carrier for 'PlaidHttp' instance.
-- We use mtl style constraints in library functions and at
-- the end call 'runPlaid' to run all operations inside of IO.
-- You never need to construct value of this type it is just used
-- as a monad stack to carry Plaid operations.
newtype Plaid a =
  Plaid
    { unPlaid :: ReaderT PlaidEnv IO a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadReader PlaidEnv
                       , MonadIO
                       , MonadThrow
                       )

-- | Carrier type used for testing
newtype PlaidTest a =
  PlaidTest
    { unPlaidTest :: Identity a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       )

runPlaid :: PlaidEnv -> Plaid a -> IO a
runPlaid env = flip runReaderT env . unPlaid

runTestPlaid :: PlaidTest a -> a
runTestPlaid = runIdentity . unPlaidTest

data CurrencyCode
  = USD
  | EUR
  deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 12 }) ''CurrencyCode)

data Balance =
  Balance
    { balanceAvailable              :: Double
    , balanceCurrent                :: Double
    , balanceLimit                  :: Maybe Double
    , balanceIsoCurrencyCode        :: CurrencyCode
    , balanceUnofficialCurrencyCode :: Maybe CurrencyCode
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 7 }) ''Balance)

data Account =
  Account
    { accountAccountId    :: Text
    , accountBalances     :: Balance
    , accountMask         :: Text
    , accountName         :: Text
    , accountOfficialName :: Text
    , accountSubtype      :: Text
    , accountType         :: Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 7 }) ''Account)

data Ach =
  Ach
    { achAccount     :: AccountNumber
    , achAccountId   :: AccountId
    , achRouting     :: Text
    , achWireRouting :: Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 3 }) ''Ach)

data Error =
  Error
    { errorType      :: Text
    , errorCode      :: Text
    , errorMessage   :: Text
    , displayMessage :: Maybe Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 5 }) ''Error)

data Item =
  Item
    { itemAvailableProducts :: [Text]
    , itemBilledProducts    :: [Text]
    , itemError             :: Maybe Error
    , itemInstitutionId     :: InstitutionId
    , itemItemId            :: Text
    , itemWebhook           :: Maybe Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 4 }) ''Item)

data Eft =
  Eft
    { eftAccount     :: AccountNumber
    , eftAccountId   :: AccountId
    , eftInstitution :: Institution
    , eftBranch      :: Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 3 }) ''Eft)

data International =
  International
    { internationalAccountId :: AccountId
    , internationalBic       :: Bic
    , internationalIban      :: Iban
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 13 }) ''International)

data Bacs =
  Bacs
    { bacsAccount   :: AccountNumber
    , bacsAccountId :: AccountId
    , bacsSortCode  :: SortCode
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 4 }) ''Bacs)

data Numbers =
  Numbers
    { numbersAch           :: [Ach]
    , numbersEft           :: [Eft]
    , numbersInternational :: [International]
    , numbersBacs          :: [Bacs]
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 7 }) ''Numbers)

data TransactionType
  = Digital
  | Place
  | Special
  | Unresolved
  deriving (Eq, Show)

instance ToJSON TransactionType where
  toJSON Digital    = String "digital"
  toJSON Place      = String "place"
  toJSON Special    = String "special"
  toJSON Unresolved = String "unresolved"

instance FromJSON TransactionType where
  parseJSON "digital"    = pure Digital
  parseJSON "place"      = pure Place
  parseJSON "special"    = pure Special
  parseJSON "unresolved" = pure Unresolved
  parseJSON _            = mempty

data TransactionLocation =
  TransactionLocation
    { transactionLocationAddress       :: Maybe Text
    , transactionLocationCity          :: Maybe Text
    , transactionLocationRegion        :: Maybe Text
    , transactionLocationPostalCode    :: Maybe Text
    , transactionLocationPostalCountry :: Maybe Text
    , transactionLocationPostalLat     :: Maybe Double
    , transactionLocationPostalLon     :: Maybe Double
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 19 }) ''TransactionLocation)

data PaymentMeta =
  PaymentMeta
    { paymentMetaReferenceNumber :: Maybe Text
    , paymentMetaPpdId           :: Maybe Text
    , paymentMetaPayee           :: Maybe Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 11 }) ''PaymentMeta)

data Transaction =
  Transaction
    { transactionAccountId              :: AccountId
    , transactionAmount                 :: Double
    , transactionIsoCurrencyCode        :: CurrencyCode
    , transactionUnofficialCurrencyCode :: Maybe CurrencyCode
    , transactionCategory               :: Maybe [Text]
    , transactionCategoryId             :: Maybe Text
    , transactionTransactionType        :: Maybe TransactionType
    , transactionName                   :: Text
    , transactionDate                   :: Day
    , transactionLocation               :: TransactionLocation
    , transactionPending                :: Bool
    , transactionPaymentMeta            :: PaymentMeta
    , transactionPendingTransactionId   :: Maybe Text
    , transactionAccountOwner           :: Maybe Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 11 }) ''Transaction)

data PlaidTransactionsGetResponse =
  PlaidTransactionsGetResponse
    { _plaidTransactionsGetResponseAccounts     :: [Account]
    , _plaidTransactionsGetResponseTransactions :: [Transaction]
    , _plaidTransactionsItem                    :: Item
    , _plaidTransactionsTotalTransactions       :: Int
    , _plaidTransactionsRequestId               :: Text
    } deriving (Eq, Show)

makeLenses ''PlaidTransactionsGetResponse

$(deriveToJSON (defaultOptions { fieldLabelModifier = camel . drop 29 }) ''PlaidTransactionsGetResponse)

instance FromJSON PlaidTransactionsGetResponse where
  parseJSON = withObject "PlaidTransactionsGetResponse" $ \o ->
    PlaidTransactionsGetResponse
      <$> o .: "accounts"
      <*> o .: "transactions"
      <*> o .: "item"
      <*> o .: "total_transactions"
      <*> o .: "request_id"

data PlaidAuthGetResponse =
  PlaidAuthGetResponse
    { _plaidAuthGetResponseAccounts  :: [Account]
    , _plaidAuthGetResponseNumbers   :: Numbers
    , _plaidAuthGetResponseItem      :: Item
    , _plaidAuthGetResponseRequestId :: RequestId
    } deriving (Eq, Show)

makeLenses ''PlaidAuthGetResponse

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 21 }) ''PlaidAuthGetResponse)

data PlaidPublicTokenResponse =
  PlaidPublicTokenResponse
    { _plaidPublicTokenResponsePublicToken :: PublicToken
    , _plaidPublicTokenResponseRequestId   :: Text
    } deriving (Eq, Show)

makeLenses ''PlaidPublicTokenResponse

instance ToJSON PlaidPublicTokenResponse where
  toJSON PlaidPublicTokenResponse {..} =
    object [ "public_token" .= _plaidPublicTokenResponsePublicToken
           , "request_id"   .= _plaidPublicTokenResponseRequestId
           ]

instance FromJSON PlaidPublicTokenResponse where
  parseJSON = withObject "PlaidPublicTokenResponse" $ \o ->
    PlaidPublicTokenResponse
      <$> o .: "public_token"
      <*> o .: "request_id"

data PlaidAccessTokenResponse =
  PlaidAccessTokenResponse
    { _plaidAccessTokenResponseAccessToken :: AccessToken
    , _plaidAccessTokenResponseRequestId   :: Text
    , _plaidAccessTokenResponseItemId      :: Text
    } deriving (Eq, Show)

makeLenses ''PlaidAccessTokenResponse

instance ToJSON PlaidAccessTokenResponse where
  toJSON PlaidAccessTokenResponse {..} =
    object [ "access_token" .= _plaidAccessTokenResponseAccessToken
           , "request_id"   .= _plaidAccessTokenResponseRequestId
           , "item_id"      .= _plaidAccessTokenResponseItemId
           ]

instance FromJSON PlaidAccessTokenResponse where
  parseJSON = withObject "PlaidAccessTokenResponse" $ \o ->
    PlaidAccessTokenResponse
      <$> o .: "access_token"
      <*> o .: "request_id"
      <*> o .: "item_id"

data PhoneNumber =
  PhoneNumber
    { _phoneNumberData    :: Text
    , _phoneNumberPrimary :: Bool
    , _phoneNumberType    :: Text
    } deriving (Eq, Show)

makeLenses ''PhoneNumber

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 12 }) ''PhoneNumber)

data Email =
  Email
    { _emailData    :: Text
    , _emailPrimary :: Bool
    , _emailType    :: Text
    } deriving (Eq, Show)

makeLenses ''Email

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 6 }) ''Email)

data Address =
  Address
    { _addressCity       :: Text
    , _addressRegion     :: Text
    , _addressStreet     :: Text
    , _addressPostalCode :: Text
    , _addressCountry    :: Maybe Text
    } deriving (Eq, Show)

makeLenses ''Address

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 8 }) ''Address)

data Addresses =
  Addresses
    { _addressesData    :: Address
    , _addressesPrimary :: Bool
    } deriving (Eq, Show)

makeLenses ''Addresses

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 10 }) ''Addresses)

data Owners =
  Owners
    { _ownersAddresses    :: [Addresses]
    , _ownersEmails       :: [Email]
    , _ownersNames        :: [Text]
    , _ownersPhoneNumbers :: [PhoneNumber]
    } deriving (Eq, Show)

makeLenses ''Owners

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 7 }) ''Owners)

data Accounts =
  Accounts
    { _accountsAccountId    :: Text
    , _accountsBalances     :: Balance
    , _accountsMask         :: Text
    , _accountsName         :: Text
    , _accountsOfficialName :: Maybe Text
    , _accountsOwners       :: [Owners]
    , _accountsSubtype      :: Text
    , _accountsType         :: Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = quietSnake . drop 9 }) ''Accounts)

data PlaidIdentityGetResponse =
  PlaidIdentityGetResponse
    { _plaidIdentityGetResponseAccounts :: [Accounts]
    , _plaidIdentityGetItem             :: Item
    , _plaidIdentityGetRequestId        :: Text
    } deriving (Eq, Show)

makeLenses ''PlaidIdentityGetResponse

$(deriveToJSON (defaultOptions { fieldLabelModifier = camel . drop 25 }) ''PlaidIdentityGetResponse)

instance FromJSON PlaidIdentityGetResponse where
  parseJSON = withObject "PlaidIdentityGetResponse" $ \o ->
    PlaidIdentityGetResponse
      <$> o .: "accounts"
      <*> o .: "item"
      <*> o .: "request_id"

data IncomeStream =
  IncomeStream
    { _incomeStreamMonthlyIncome :: Fixed E2
    , _incomeStreamConfidence    :: Fixed E2
    , _incomeStreamDays          :: Fixed E2
    , _incomeStreamName          :: Text
    } deriving (Eq, Show)

makeLenses ''IncomeStream

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 13 }) ''IncomeStream)

data Income =
  Income
    { _incomeLastYearIncome                      :: Fixed E2
    , _incomeLastYearIncomeBeforeTax             :: Fixed E2
    , _incomeProjectedYearlyIncome               :: Fixed E2
    , _incomeProjectedYearlyIncomeBeforeTax      :: Fixed E2
    , _incomeIncomeStreams                       :: [IncomeStream]
    , _incomeMaxNumberOfOverlappingIncomeStreams :: Fixed E2
    , _incomeNumberOfIncomeStreams               :: Fixed E2
    } deriving (Eq, Show)

makeLenses ''Income

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 7 }) ''Income)

baseUrl :: Environment -> Url Environment
baseUrl e = Url $ T.pack $ "https://" <> show e <> ".plaid.com"

envUrl :: Environment -> Text
envUrl = unUrl . baseUrl

validInstitutionIds :: [InstitutionId]
validInstitutionIds =
  InstitutionId <$>
    [ "ins_1"
    , "ins_2"
    , "ins_3"
    , "ins_4"
    , "ins_5"
    , "ins_6"
    , "ins_7"
    , "ins_9"
    , "ins_10"
    , "ins_11"
    , "ins_13"
    , "ins_14"
    , "ins_15"
    , "ins_16"
    , "ins_19"
    , "ins_20"
    , "ins_21"
    , "ins_23"
    , "ins_24"
    , "ins_27"
    , "ins_29"
    ]

-- | Map used for testing different plaid endpoints
requestMap :: M.Map Text ByteString
requestMap =
  M.fromList
    [ ("/auth/get", responseAuthGet)
    , ("/transactions/get", responseTransactionsGet)
    , ("/public_token/create", responsePublicTokenCreate)
    , ("/item/public_token/exchange", responsePublicTokenExchange)
    , ("/identity/get", identityJson)
    ]

-- | Instances
instance PlaidHttp Plaid where
  executeGet :: Text -> Plaid ByteString
  executeGet url = do
    request <- parseUrlThrow (T.unpack url)
    m <- liftIO $ newManager tlsManagerSettings
    response <- httpLbs request m
    return $ responseBody response

  executePost :: ToJSON a => Text -> a -> Plaid ByteString
  executePost url postBody = do
    initialRequest <- parseUrlThrow (T.unpack url)
    m <- liftIO (newManager tlsManagerSettings)
    let request =
          initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS (encode postBody)
          , requestHeaders = [ ("Content-Type", "application/json") ]
          }
    response <- httpLbs request m
    return $ responseBody response

instance PlaidHttp PlaidTest where
  executeGet :: Text -> PlaidTest ByteString
  executeGet url =
    case M.lookup url requestMap of
      Nothing -> return ""
      Just bs -> return bs

  executePost :: ToJSON a => Text -> a -> PlaidTest ByteString
  executePost url _postBody =
    case M.lookup url requestMap of
      Nothing -> return ""
      Just bs -> return bs

instance Semigroup PlaidOptions where
  (<>) _ b = b

instance Monoid PlaidOptions where
  mempty =
    PlaidOptions
      { _plaidOptionsWebHook          = ""
      , _plaidOptionsOverrideUsername = ""
      , _plaidOptionsOverridePassword = ""
      }
  mappend _ b = b

instance Semigroup (PlaidBody a) where
  (<>) _ b = b

instance Monoid (PlaidBody a) where
  mempty =
    let env =
          PlaidEnv
            { _plaidEnvPublicKey   = ""
            , _plaidEnvClientId    = ClientId ""
            , _plaidEnvSecret      = Secret ""
            , _plaidEnvEnvironment = Sandbox
            }
    in
      PlaidBody
        { _plaidBodyEnv               = env
        , _plaidBodyPublicToken       = Nothing
        , _plaidBodyAccessToken       = Nothing
        , _plaidBodyOptions           = Nothing
        , _plaidBodyPaginationOptions = Nothing
        , _plaidBodyInstitutionId     = Nothing
        , _plaidBodyInitialProducts   = Nothing
        , _plaidBodyStartDate         = Nothing
        , _plaidBodyEndDate           = Nothing
        }
  mappend _ b = b

