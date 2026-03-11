{-# LANGUAGE RecordWildCards #-}

-- | CSV serialization of BNB Staking Rewards.
module Console.BnbStaking.Csv
    ( makeCsvContents
    , ExportData (..)
    , convertReward
    , MyZonedTime (..)
    ) where

import Data.Csv
    ( DefaultOrdered
    , ToField (..)
    , ToNamedRecord
    , encodeDefaultOrderedByName
    )
import Data.Scientific
    ( FPFormat (..)
    , formatScientific
    )
import Data.Time
    ( ZonedTime (..)
    , defaultTimeLocale
    , formatTime
    , utcToLocalZonedTime
    )
import GHC.Generics (Generic)

import Console.BnbStaking.Api (Reward (..))

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T


-- | Build the CSV contents for the given rewards, including the header
-- row.
makeCsvContents :: [Reward] -> IO LBS.ByteString
makeCsvContents = fmap encodeDefaultOrderedByName . mapM convertReward


-- | Datatype representing a single row in the CSV export.
data ExportData = ExportData
    { time :: MyZonedTime
    -- ^ The time of the reward.
    , amount :: T.Text
    -- ^ The reward amount.
    , currency :: T.Text
    -- ^ Always @BNB@, but sometimes a useful column for CSV imports.
    , delegator :: T.Text
    -- ^ The address that was rewarded.
    , validator :: T.Text
    -- ^ The validator's name.
    , validatorAddress :: T.Text
    -- ^ The address the delegator is validating to.
    , height :: Integer
    -- ^ The height of the reward's block.
    }
    deriving (Show, Read, Generic)


instance ToNamedRecord ExportData
instance DefaultOrdered ExportData


-- | Render a 'Reward' into our target export data by converting to
-- localtime(respecting DST), & formatting the amount column to 8 decimal
-- places.
convertReward :: Reward -> IO ExportData
convertReward Reward {..} = do
    localRewardTime <- utcToLocalZonedTime rRewardTime
    return $
        ExportData
            { time = MyZonedTime localRewardTime
            , amount = T.pack $ formatScientific Fixed (Just 8) rReward
            , currency = "BNB"
            , delegator = rDelegator
            , validator = rValidatorName
            , validatorAddress = rValidatorAddress
            , height = rHeight
            }


-- | Wrapper type to support custom 'ToField' instance.
newtype MyZonedTime = MyZonedTime {fromMyZonedTime :: ZonedTime} deriving (Show, Read)


-- | Render with @%FT%T%Q%Ez@ formatting string.
instance ToField MyZonedTime where
    toField (MyZonedTime zt) =
        toField $ formatTime defaultTimeLocale "%FT%T%Q%Ez" zt
