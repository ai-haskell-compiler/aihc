{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-| Request functions & response types for the Gemini Exchange API.
-}
module Web.Gemini
    ( GeminiApiM
    , runApi
    , GeminiConfig(..)
    , GeminiError(..)
    -- * Requests
    -- ** Symbol Details
    , getSymbolDetails
    , SymbolDetails(..)
    -- ** Trade History
    , getMyTrades
    , Trade(..)
    -- ** Transfer History
    , getMyTransfers
    , Transfer(..)
    -- ** Earn History
    , getMyEarnTransactions
    , EarnHistory(..)
    , EarnTransaction(..)
    -- * Helpers
    , protectedGeminiRequest
    , retryWithRateLimit
    , createSignature
    , makeNonce
    ) where

import           Control.Concurrent             ( threadDelay )
import           Control.Exception.Safe         ( MonadCatch
                                                , MonadThrow
                                                , try
                                                )
import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , MonadReader(ask)
                                                , ReaderT(..)
                                                , lift
                                                )
import           Crypto.Hash                    ( SHA384 )
import           Crypto.MAC.HMAC                ( hmac
                                                , hmacGetDigest
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                , eitherDecode
                                                , encode
                                                , withObject
                                                )
import           Data.Base64.Types              ( extractBase64 )
import           Data.ByteString.Base64         ( encodeBase64' )
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                , mapMaybe
                                                )
import           Data.Ratio                     ( (%) )
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time                      ( UTCTime )
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , getPOSIXTime
                                                , utcTimeToPOSIXSeconds
                                                )
import           Data.Version                   ( showVersion )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Client            ( HttpException(..)
                                                , HttpExceptionContent(..)
                                                , responseStatus
                                                )
import           Network.HTTP.Req               ( (/:)
                                                , GET(..)
                                                , HttpBodyAllowed
                                                , HttpException(..)
                                                , HttpMethod(..)
                                                , JsonResponse
                                                , MonadHttp(..)
                                                , NoReqBody(..)
                                                , Option
                                                , POST(..)
                                                , ProvidesBody
                                                , Req
                                                , Url
                                                , defaultHttpConfig
                                                , header
                                                , https
                                                , jsonResponse
                                                , req
                                                , responseBody
                                                , runReq
                                                )
import           Network.HTTP.Types             ( Status(..) )
import           Text.Read                      ( readMaybe )

import           Paths_gemini_exports           ( version )

import qualified Data.Aeson.KeyMap             as KM
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T


-- | Required configuration data for making requests to the Gemini API.
data GeminiConfig = GeminiConfig
    { gcApiKey    :: Text
    , gcApiSecret :: Text
    }
    deriving (Show, Read, Eq, Ord)

-- | Monad in which Gemini API requests are run.
newtype GeminiApiM a = GeminiApiM
    { runGeminiApiM :: ReaderT GeminiConfig Req a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GeminiConfig, MonadThrow, MonadCatch)

-- | Run a series of API requests with the given Config.
runApi :: GeminiConfig -> GeminiApiM a -> IO a
runApi cfg = runReq defaultHttpConfig . flip runReaderT cfg . runGeminiApiM

-- | Use 'MonadHttp' from the 'Req' monad.
instance MonadHttp GeminiApiM where
    handleHttpException = GeminiApiM . lift . handleHttpException

-- | Potential error response body from the API.
data GeminiError = GeminiError
    { geReason  :: Text
    , geMessage :: Text
    }
    deriving (Show, Read, Eq, Ord)

instance FromJSON GeminiError where
    parseJSON = withObject "GeminiError"
        $ \o -> GeminiError <$> o .: "reason" <*> o .: "message"


-- SYMBOL DETAILS

-- | Fetch the details on a supported symbol.
getSymbolDetails :: MonadHttp m => Text -> m SymbolDetails
getSymbolDetails symbol =
    responseBody
        <$> req
                GET
                (  https "api.gemini.com"
                /: "v1"
                /: "symbols"
                /: "details"
                /: symbol
                )
                NoReqBody
                jsonResponse
                userAgentHeader

-- | Currency & Precision details for a 'Trade' Symbol.
data SymbolDetails = SymbolDetails
    { sdSymbol         :: Text
    , sdBaseCurrency   :: Text
    , sdBasePrecision  :: Scientific
    , sdQuoteCurrency  :: Text
    , sdQuotePrecision :: Scientific
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON SymbolDetails where
    parseJSON = withObject "SymbolDetails" $ \o -> do
        sdSymbol         <- o .: "symbol"
        sdBaseCurrency   <- o .: "base_currency"
        sdBasePrecision  <- o .: "tick_size"
        sdQuoteCurrency  <- o .: "quote_currency"
        sdQuotePrecision <- o .: "quote_increment"
        return SymbolDetails { .. }


-- TRADE HISTORY

-- | Fetch all my Gemini Trades
getMyTrades
    :: Maybe (UTCTime, UTCTime)
    -- ^ Optional @(start, end)@ ranges for fetching.
    -> GeminiApiM [Trade]
getMyTrades = fetchAllPages getTradeBatch tTimestamp
  where
    getTradeBatch :: Integer -> GeminiApiM [Trade]
    getTradeBatch timestamp = do
        nonce <- makeNonce
        let parameters = KM.fromList
                [ ("request"     , String "/v1/mytrades")
                , ("nonce"       , toJSON nonce)
                , ("timestamp"   , toJSON $ timestampToSeconds timestamp)
                , ("limit_trades", Number 500)
                ]
        responseBody
            <$> protectedGeminiRequest
                    POST
                    (https "api.gemini.com" /: "v1" /: "mytrades")
                    parameters

-- | A single, completed Trade.
data Trade = Trade
    { tId          :: Integer
    , tSymbol      :: Text
    , tPrice       :: Scientific
    , tAmount      :: Scientific
    , tFeeCurrency :: Text
    , tFeeAmount   :: Scientific
    , tIsBuy       :: Bool
    , tIsAggressor :: Bool
    , tTimestamp   :: POSIXTime
    , tOrderId     :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Trade where
    parseJSON = withObject "Trade" $ \o -> do
        tId          <- o .: "tid"
        tSymbol      <- o .: "symbol"
        tPrice       <- read <$> o .: "price"
        tAmount      <- read <$> o .: "amount"
        tFeeCurrency <- o .: "fee_currency"
        tFeeAmount   <- read <$> o .: "fee_amount"
        tIsBuy       <- (== ("Buy" :: String)) <$> o .: "type"
        tIsAggressor <- o .: "aggressor"
        tTimestamp   <- (/ 1000.0) <$> o .: "timestampms"
        tOrderId     <- o .: "order_id"
        return Trade { .. }


-- TRANSFER HISTORY

-- | Fetch all my Gemini Transfers
getMyTransfers
    :: Maybe (UTCTime, UTCTime)
    -- ^ Optional @(start, end)@ ranges for fetching.
    -> GeminiApiM [Transfer]
getMyTransfers = fetchAllPages getTransferBatch trTimestamp
  where
    getTransferBatch :: Integer -> GeminiApiM [Transfer]
    getTransferBatch timestamp = do
        nonce <- makeNonce
        let parameters = KM.fromList
                [ ("request"        , String "/v1/transfers")
                , ("nonce"          , toJSON nonce)
                , ("timestamp"      , toJSON $ timestampToSeconds timestamp)
                , ("limit_transfers", Number 50)
                ]
        responseBody
            <$> protectedGeminiRequest
                    POST
                    (https "api.gemini.com" /: "v1" /: "transfers")
                    parameters

-- | A single fiat or cryptocurrency transfer, credit, deposit, or withdrawal.
data Transfer = Transfer
    { trId        :: Integer
    , trType      :: Text
    , trStatus    :: Text
    , trCurrency  :: Text
    , trAmount    :: Scientific
    , trMethod    :: Maybe Text
    , trPurpose   :: Maybe Text
    , trTimestamp :: POSIXTime
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Transfer where
    parseJSON = withObject "Transfer" $ \o -> do
        trId        <- o .: "eid"
        trType      <- o .: "type"
        trStatus    <- o .: "status"
        trCurrency  <- o .: "currency"
        trAmount    <- read <$> o .: "amount"
        trMethod    <- o .:? "method"
        trPurpose   <- o .:? "purpose"
        trTimestamp <- (/ 1000) <$> o .: "timestampms"
        return Transfer { .. }


-- EARN HISTORY

-- | Fetch all my Gemini Earn Transactions
getMyEarnTransactions
    :: Maybe (UTCTime, UTCTime) -> GeminiApiM [EarnTransaction]
getMyEarnTransactions = fetchAllPages getEarnBatch etTimestamp
  where
    getEarnBatch :: Integer -> GeminiApiM [EarnTransaction]
    getEarnBatch timestamp = do
        nonce <- makeNonce
        let parameters = KM.fromList
                [ ("request", String "/v1/earn/history")
                , ("nonce"  , toJSON nonce)
                , ("since"  , toJSON timestamp)
                , ("sortAsc", toJSON True)
                , ("limit"  , Number 500)
                ]
        concatMap @[] ehTransactions
            .   responseBody
            <$> protectedGeminiRequest
                    POST
                    (https "api.gemini.com" /: "v1" /: "earn" /: "history")
                    parameters

-- | Earn Transactions grouped by a Provider/Borrower.
data EarnHistory = EarnHistory
    { ehProviderId   :: Text
    , ehTransactions :: [EarnTransaction]
    }

instance FromJSON EarnHistory where
    parseJSON = withObject "EarnHistory"
        $ \o -> EarnHistory <$> o .: "providerId" <*> o .: "transactions"

-- | A single Earn transaction.
data EarnTransaction = EarnTransaction
    { etId             :: Text
    , etType           :: Text
    , etAmountCurrency :: Text
    , etAmount         :: Scientific
    , etPriceCurrency  :: Maybe Text
    , etPrice          :: Maybe Scientific
    , etTimestamp      :: POSIXTime
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON EarnTransaction where
    parseJSON = withObject "EarnTransaction" $ \o -> do
        etId             <- o .: "earnTransactionId"
        etType           <- o .: "transactionType"
        etAmountCurrency <- o .: "amountCurrency"
        etAmount         <- o .: "amount"
        etPriceCurrency  <- o .:? "priceCurrency"
        etPrice          <- o .:? "priceAmount"
        etTimestamp      <- (/ 1000.0) <$> o .: "dateTime"
        return EarnTransaction { .. }


-- UTILS

-- | Run a request that requires authorization against the Gemini API.
protectedGeminiRequest
    :: ( MonadHttp m
       , HttpMethod method
       , HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)
       , ToJSON body
       , FromJSON response
       , MonadReader GeminiConfig m
       )
    => method
    -> Url scheme
    -> body
    -> m (JsonResponse response)
protectedGeminiRequest method url body = do
    cfg <- ask
    let payload   = extractBase64 . encodeBase64' . LBS.toStrict $ encode body
        signature = createSignature cfg payload
    let authorizedOptions = mconcat
            [ header "Content-Type"       "text/plain"
            , header "X-GEMINI-APIKEY"    (encodeUtf8 $ gcApiKey cfg)
            , header "X-GEMINI-PAYLOAD"   payload
            , header "X-GEMINI-SIGNATURE" signature
            , header "Cache-Control"      "no-cache"
            , userAgentHeader
            ]
    req method url NoReqBody jsonResponse authorizedOptions

-- | Attempt a request & retry if a @429@ @RateLimited@ error is returned.
-- We attempt to parse the retry wait time from the @message@ field but
-- fallback to one second.
retryWithRateLimit :: (MonadHttp m, MonadCatch m) => m a -> m a
retryWithRateLimit request = try request >>= \case
    Left e@(VanillaHttpException (HttpExceptionRequest _ (StatusCodeException (statusCode . responseStatus -> 429) body)))
        -> case eitherDecode $ LBS.fromStrict body of
            Left  _ -> handleHttpException e
            Right r -> if geReason r == "RateLimited"
                then
                    let msToWait =
                            fromMaybe 1000
                                . listToMaybe
                                . mapMaybe (readMaybe . T.unpack)
                                . T.words
                                $ geMessage r
                    in  do
                            liftIO . threadDelay $ msToWait * 1000
                            retryWithRateLimit request
                else handleHttpException e
    Left  e -> handleHttpException e
    Right r -> return r

-- | Fetch all pages of a response by calling the API with increasing
-- timestamp fields until it returns an empty response. Takes an optional
-- start & end date to offset the initial fetch & stop fetching early.
fetchAllPages
    :: (Integer -> GeminiApiM [a])
    -- ^ Make a request for items above the given milliseconds @timestamp@
    -> (a -> POSIXTime)
    -- ^ Pull a timestamp from an item
    -> Maybe (UTCTime, UTCTime)
    -- ^ Optional @(start, end)@ range
    -> GeminiApiM [a]
fetchAllPages mkRequest getTimestamp mbRange = do
    let startTimestamp =
            maybe 0 (truncate . (1000 *) . utcTimeToPOSIXSeconds . fst) mbRange
    fetchAll [] startTimestamp
  where
    fetchAll prevResults timestamp = do
        newResults <- retryWithRateLimit $ mkRequest timestamp
        if null newResults
            then return prevResults
            else
                let
                    maxTimestamp  = maximum $ map getTimestamp newResults
                    nextTimestamp = truncate $ 1000 * maxTimestamp + 1
                    continueFetching =
                        fetchAll (newResults <> prevResults) nextTimestamp
                    filteredResults end =
                        filter ((<= end) . getTimestamp) newResults
                in
                    case mbRange of
                        Nothing -> continueFetching
                        Just (_, utcTimeToPOSIXSeconds -> end) ->
                            if maxTimestamp >= end
                                then return $ filteredResults end <> prevResults
                                else continueFetching

-- | Given a timestamp in ms, convert it to a timestamp param in seconds by
-- dividing & rounding up.
timestampToSeconds :: Integer -> Integer
timestampToSeconds = ceiling . (% 1000)

-- | Generate a 'Crypto.MAC.HMAC.HMAC' 'SHA384' signature for an authorized
-- API request.
createSignature
    :: GeminiConfig
    -- ^ API Credentials
    -> BS.ByteString
    -- ^ Base64-encoded request body.
    -> BS.ByteString
createSignature cfg body =
    let digest =
            hmacGetDigest @SHA384 $ hmac (encodeUtf8 $ gcApiSecret cfg) body
    in  BC.pack $ show digest


-- | Generate a nonce for authorized requests from the current timestamp in
-- milliseconds.
makeNonce :: MonadIO m => m Integer
makeNonce = truncate . (1000 *) <$> liftIO getPOSIXTime


-- | Generate a @User-Agent@ header with the library's current version.
userAgentHeader :: Option scheme
userAgentHeader =
    header "User-Agent" . BC.pack $ "gemini-exports/v" <> showVersion version
