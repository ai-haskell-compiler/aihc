{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

-- | Binance.org API requests & responses.
module Console.BnbStaking.Api
    ( -- * Rewards
      getAllRewards
    , Reward (..)

      -- * Low-Level Requests & Responses
    , Endpoint (..)
    , makeRequest
    , RewardResponse (..)
    ) where

import Control.Monad (forM)
import Data.Aeson
    ( FromJSON (..)
    , withObject
    , (.:)
    )
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , parseTimeM
    )
import GHC.Generics (Generic)
import Network.HTTP.Req
    ( GET (..)
    , MonadHttp
    , NoReqBody (..)
    , Scheme (Https)
    , Url
    , https
    , jsonResponse
    , req
    , responseBody
    , (/:)
    , (=:)
    )

import Data.Text qualified as T


-- | Fetch all rewards for the given Delegator PubKey.
getAllRewards :: (MonadHttp m) => T.Text -> m [Reward]
getAllRewards pubKey = do
    let pageSize = 50
        jPageSize = Just pageSize
    initialResp <- makeRequest $ GetRewards pubKey jPageSize Nothing
    let rewardCount = rrTotal initialResp
    remainingRewards <-
        if rewardCount < pageSize
            then return []
            else fmap concat
                . forM [pageSize, pageSize * 2 .. rewardCount]
                $ \(Just -> offset) ->
                    rrRewards <$> makeRequest (GetRewards pubKey jPageSize offset)
    return . sortResults $ rrRewards initialResp <> remainingRewards
  where
    sortResults :: [Reward] -> [Reward]
    sortResults = sortOn rRewardTime


-- | Represents all endpoints of the binance.org api, as well as their
-- respective response data.
data Endpoint a where
    GetRewards :: T.Text -> Maybe Integer -> Maybe Integer -> Endpoint RewardResponse


-- | Make a request to an endpoint.
makeRequest :: (MonadHttp m) => Endpoint a -> m a
makeRequest e = case e of
    GetRewards _ mbLimit mbOffset ->
        responseBody
            <$> req
                GET
                url
                NoReqBody
                jsonResponse
                ( ("limit" =: fromMaybe 20 mbLimit) <> ("offset" =: fromMaybe 0 mbOffset)
                )
  where
    url :: Url 'Https
    url = case e of
        GetRewards pubKey _ _ ->
            baseUrl
                /: "staking"
                /: "chains"
                /: "bsc"
                /: "delegators"
                /: pubKey
                /: "rewards"
    baseUrl :: Url 'Https
    baseUrl = https "api.binance.org" /: "v1"


-- | Response of requesting a delegator's rewards.
data RewardResponse = RewardResponse
    { rrTotal :: Integer
    -- ^ Total number of rewards.
    , rrRewards :: [Reward]
    -- ^ Rewards in this page.
    }
    deriving (Show, Read, Eq, Generic)


instance FromJSON RewardResponse where
    parseJSON = withObject "RewardResponse" $ \o -> do
        rrTotal <- o .: "total"
        rrRewards <- o .: "rewardDetails"
        return $ RewardResponse {..}


-- | A single staking reward.
data Reward = Reward
    { rValidatorName :: T.Text
    , rValidatorAddress :: T.Text
    , rDelegator :: T.Text
    , rChainId :: T.Text
    -- ^ Always @bsc@ at the moment - no testnet rewards supported.
    , rHeight :: Integer
    , rReward :: Scientific
    , rRewardTime :: UTCTime
    }
    deriving (Show, Read, Eq, Generic)


instance FromJSON Reward where
    parseJSON = withObject "Reward" $ \o -> do
        rValidatorName <- o .: "valName"
        rValidatorAddress <- o .: "validator"
        rDelegator <- o .: "delegator"
        rChainId <- o .: "chainId"
        rHeight <- o .: "height"
        rReward <- o .: "reward"
        rRewardTime <-
            o .: "rewardTime"
                >>= parseTimeM
                    True
                    defaultTimeLocale
                    "%FT%T%Q%Ez"
        return $ Reward {..}
