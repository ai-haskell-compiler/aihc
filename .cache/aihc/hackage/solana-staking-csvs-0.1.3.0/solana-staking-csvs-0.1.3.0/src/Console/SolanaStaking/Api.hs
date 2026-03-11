{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Solana Beach API requests & responses.

TODO: Extract into a @solana-beach-api@ package.
-}
module Console.SolanaStaking.Api
    ( -- * Configuration
      Config (..)
    , mkConfig

      -- * Requests / Responses
    , APIResponse (..)
    , APIError (..)
    , runApi
    , raiseAPIError

      -- ** Get Stake Accounts
    , getAccountStakes
    , StakingAccounts (..)
    , StakingAccount (..)

      -- ** Get Staking Rewards
    , getAllStakeRewards
    , getYearsStakeRewards
    , getStakeRewards
    , StakeReward (..)

      -- ** Get Block
    , getBlock
    , Block (..)

      -- * General API Types
    , Lamports (..)
    , renderLamports
    , scientificLamports
    , StakingPubKey (..)
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson
    ( FromJSON (parseJSON)
    , Value (Object)
    , eitherDecode
    , withObject
    , (.:)
    , (.:?)
    )
import Data.Bifunctor (second)
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (toGregorian, utctDay)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent (..)
    , responseStatus
    )
import Network.HTTP.Req
    ( GET (GET)
    , HttpException (..)
    , HttpResponse
    , HttpResponseBody
    , NoReqBody (NoReqBody)
    , Option
    , Scheme (Https)
    , Url
    , defaultHttpConfig
    , header
    , https
    , jsonResponse
    , queryParam
    , renderUrl
    , req
    , responseBody
    , runReq
    , (/:)
    , (/~)
    )
import Network.HTTP.Types (statusCode)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T


-- | Solana Beach API Configuration
data Config = Config
    { cApiKey :: T.Text
    -- ^ Your API Key.
    -- Get one here: https://github.com/solana-beach/api
    , cAccountPubKey :: T.Text
    -- ^ TODO: probably drop this when solana-beach-api is extracted to
    -- separate package.
    }
    deriving (Show, Read, Eq)


-- | Create a program config from the API key & the target account's
-- pubkey.
mkConfig :: String -> String -> Config
mkConfig (T.pack -> cApiKey) (T.pack -> cAccountPubKey) = Config {..}


-- | Base URL to Solana Beach's API
baseUrl :: Url 'Https
baseUrl = https "api.solanabeach.io" /: "v1"


-- | Get the staking accounts for the 'cAccountPubKey'.
getAccountStakes
    :: (MonadReader Config m, MonadCatch m, MonadIO m)
    => m (APIResponse StakingAccounts)
getAccountStakes = do
    pubkey <- asks cAccountPubKey
    getReq (baseUrl /: "account" /~ pubkey /: "stakes") mempty


-- | Single Result Page of Staking Accounts Query.
data StakingAccounts = StakingAccounts
    { saResults :: [StakingAccount]
    -- ^ The returned staking accounts
    , saTotalPages :: Integer
    -- ^ The total number of pages for the Account's PubKey.
    }
    deriving (Show, Read, Eq, Generic)


instance FromJSON StakingAccounts where
    parseJSON = withObject "StakingAccounts" $ \o -> do
        saResults <- o .: "data"
        saTotalPages <- o .: "totalPages"
        return StakingAccounts {..}


-- | A single Staking Account.
data StakingAccount = StakingAccount
    { saPubKey :: StakingPubKey
    -- ^ The Staking Accounts PubKey
    , saLamports :: Lamports
    -- ^ The Balance of the Staking Account
    , saValidatorName :: T.Text
    -- ^ The Name of the Staking Account's Validator
    }
    deriving (Show, Read, Eq, Generic)


instance FromJSON StakingAccount where
    parseJSON = withObject "StakingAccount" $ \o -> do
        saPubKey <- o .: "pubkey" >>= (.: "address")
        saLamports <- o .: "lamports"
        saValidatorName <-
            o
                .: "data"
                >>= (.: "stake")
                >>= (.: "delegation")
                >>= (.: "validatorInfo")
                >>= (.: "name")
        return StakingAccount {..}


-- | A PubKey for a Staking Account.
newtype StakingPubKey = StakingPubKey {fromStakingPubKey :: T.Text}
    deriving (Show, Read, Eq, Generic, FromJSON)


-- | An amount of Lamports, each of which represent 0.000000001 SOL.
newtype Lamports = Lamports {fromLamports :: Integer}
    deriving (Show, Read, Eq, Num, Generic, FromJSON)


-- | Render an amount of 'Lamports' as text, converting it to SOL.
renderLamports :: Lamports -> T.Text
renderLamports = T.pack . formatScientific Fixed (Just 9) . scientificLamports


-- | Convert Lamports into Scientific representation of SOL.
scientificLamports :: Lamports -> Scientific
scientificLamports = (* 0.000000001) . fromInteger . fromLamports


-- | Get the staking rewards with a staking account's pubkey.
getStakeRewards
    :: (MonadReader Config m, MonadCatch m, MonadIO m)
    => StakingPubKey
    -> Maybe Integer
    -> m (APIResponse [StakeReward])
getStakeRewards (StakingPubKey stakeAccountPubkey) mbEpochCursor = do
    getReq
        (baseUrl /: "account" /: stakeAccountPubkey /: "stake-rewards")
        (queryParam "cursor" mbEpochCursor)


-- | Get all the staking rewards for the given account.
--
-- The API's @stake-rewards@ route only returns a maximum of 5 rewards, so
-- we have to use the earliest epoch as the @cursor@ in an additional
-- request to see if there are any more rewards.
getAllStakeRewards
    :: (MonadReader Config m, MonadCatch m, MonadIO m)
    => StakingPubKey
    -> m ([APIError], [StakeReward])
getAllStakeRewards pubkey =
    getStakeRewards pubkey Nothing
        >>= runApi
        >>= getStakeRewardsUntil
            pubkey
            (const True)
            ([], [])


-- | Get the year's worth of staking rewards for the given account.
getYearsStakeRewards
    :: (MonadReader Config m, MonadCatch m, MonadIO m)
    => StakingPubKey
    -> Integer
    -> m ([APIError], [StakeReward])
getYearsStakeRewards pubkey year =
    fmap (second $ filter ((== year) . rewardYear)) $
        getStakeRewards pubkey Nothing
            >>= runApi
            >>= getStakeRewardsUntil pubkey stopAfterYear ([], [])
  where
    rewardYear :: StakeReward -> Integer
    rewardYear =
        (\(y, _, _) -> y)
            . toGregorian
            . utctDay
            . posixSecondsToUTCTime
            . srTimestamp

    stopAfterYear :: [StakeReward] -> Bool
    stopAfterYear rewards =
        let years = map rewardYear rewards in any (< year) years


-- | Fetch staking rewards until we get less than 5 rewards or the general
-- predicate returns true.
getStakeRewardsUntil
    :: (MonadReader Config m, MonadCatch m, MonadIO m)
    => StakingPubKey
    -> ([StakeReward] -> Bool)
    -> ([APIError], [StakeReward])
    -> Either APIError [StakeReward]
    -> m ([APIError], [StakeReward])
getStakeRewardsUntil pubkey cond (errs, rws) = \case
    Left err -> return (err : errs, rws)
    Right rewards ->
        if null rewards || cond rewards
            then return (errs, rewards <> rws)
            else
                let minEpoch = minimum $ map srEpoch rewards
                 in getStakeRewards pubkey (Just minEpoch)
                        >>= runApi
                        >>= getStakeRewardsUntil pubkey cond (errs, rewards <> rws)


-- | A Staking Reward Payment.
data StakeReward = StakeReward
    { srEpoch :: Integer
    -- ^ The Epoch the reward was paid.
    , srSlot :: Integer
    -- ^ The 'Block' number of the reward.
    , srAmount :: Lamports
    -- ^ The total number of 'Lamports' awarded.
    , srTimestamp :: POSIXTime
    }
    deriving (Show, Read, Eq, Generic)


instance FromJSON StakeReward where
    parseJSON = withObject "StakeReward" $ \o -> do
        srEpoch <- o .: "epoch"
        srSlot <- o .: "effectiveSlot"
        srAmount <- o .: "amount"
        srTimestamp <- o .: "timestamp"
        return StakeReward {..}


-- | Get information about a specific block number.
getBlock
    :: (MonadReader Config m, MonadCatch m, MonadIO m)
    => Integer
    -> m (APIResponse Block)
getBlock blockNum = do
    getReq (baseUrl /: "block" /~ blockNum) mempty


-- | A single block on the Solana blockchain.
data Block = Block
    { bNumber :: Integer
    -- ^ The blocks number.
    , bBlockTime :: POSIXTime
    -- ^ The blocks absolute timestamp.
    }
    deriving (Show, Read, Eq, Generic)


instance FromJSON Block where
    parseJSON = withObject "Block" $ \o -> do
        bNumber <- o .: "blocknumber"
        bBlockTime <-
            (o .: "blocktime")
                >>= fmap (fromInteger . (truncate @Scientific))
                    . (.: "absolute")
        return Block {..}


-- | Generic GET request to the Solana Beach API with up to 5 retries for
-- @ProcessingResponse@.
--
-- Note: Prints to 'stderr' when waiting for request to finish processing.
getReq
    :: forall m a
     . (MonadReader Config m, FromJSON a, MonadIO m, MonadCatch m)
    => Url 'Https
    -> Option 'Https
    -> m (APIResponse a)
getReq endpoint options = fetchWithRetries 0
  where
    maxRetries :: Integer
    maxRetries = 5
    fetchWithRetries :: Integer -> m (APIResponse a)
    fetchWithRetries retryCount =
        if retryCount >= maxRetries
            then return $ ErrorResponse $ RetriesExceeded $ renderUrl endpoint
            else do
                apikey <- asks cApiKey
                let authHeader =
                        header "Authorization" $ "Bearer: " <> encodeUtf8 apikey
                respBody <-
                    catchRateLimitError $
                        runReq
                            defaultHttpConfig
                            ( req GET endpoint NoReqBody jsonResponse $ authHeader <> options
                            )
                case respBody of
                    ProcessingResponse -> do
                        liftIO $
                            hPutStrLn
                                stderr
                                "Waiting for API to finish processing request..."
                        liftIO $ threadDelay $ 10 * 1000000
                        fetchWithRetries (retryCount + 1)
                    RateLimitResponse wait -> do
                        liftIO $
                            hPutStrLn stderr $
                                "Exceeded rate limit, waiting "
                                    <> show wait
                                    <> " seconds before retrying..."
                        liftIO $ threadDelay $ wait * 1000000
                        fetchWithRetries retryCount
                    _ -> return respBody
    catchRateLimitError
        :: (FromJSON (HttpResponseBody b))
        => (HttpResponse b)
        => m b
        -> m (HttpResponseBody b)
    catchRateLimitError =
        try >=> \case
            Left e@(VanillaHttpException (HttpExceptionRequest _ (StatusCodeException resp body))) ->
                if statusCode (responseStatus resp) == 429
                    then
                        either (liftIO . throwIO . JsonHttpException) return $
                            eitherDecode $
                                LBS.fromStrict body
                    else liftIO $ throwIO e
            Left e -> liftIO $ throwIO e
            Right r -> return $ responseBody r


-- | Wrapper around error & processing responses from the API.
data APIResponse a
    = SuccessfulReponse a
    | ProcessingResponse
    | RateLimitResponse Int
    | ErrorResponse APIError
    deriving (Show, Read, Eq)


-- | Attempts to parse a processing response, then an error response,
-- & finally the inner @a@ response.
instance (FromJSON a) => FromJSON (APIResponse a) where
    parseJSON v = case v of
        Object o -> do
            o .:? "err" >>= \case
                Just errMsg ->
                    o .:? "processing" >>= \case
                        Nothing ->
                            o .:? "retry" >>= \case
                                Just (readMaybe -> Just i) ->
                                    return $ RateLimitResponse i
                                _ -> return $ ErrorResponse $ APIError errMsg
                        Just (_ :: Bool) -> return ProcessingResponse
                Nothing -> fmap SuccessfulReponse . parseJSON $ Object o
        _ -> SuccessfulReponse <$> parseJSON v


-- | Evaluate an API response.
runApi :: (Monad m) => APIResponse a -> m (Either APIError a)
runApi = runExceptT . raiseAPIError


-- | Pull the inner value out of an 'APIResponse' or throw the respective
-- 'APIError'.
raiseAPIError :: (MonadError APIError m) => APIResponse a -> m a
raiseAPIError = \case
    SuccessfulReponse v -> return v
    ProcessingResponse ->
        throwError $ APIError "Request cancelled during processing wait."
    RateLimitResponse i -> throwError $ RateLimitError i
    ErrorResponse err -> throwError err


-- | Potential error responses from the Solana Beach API.
data APIError
    = -- | Generic API error with message.
      APIError T.Text
    | -- | Exceeded maximum number of 'ProcessingResponse' retries.
      RetriesExceeded T.Text
    | -- | Rate limiting 429 error.
      RateLimitError Int
    deriving (Show, Read, Eq, Generic)
