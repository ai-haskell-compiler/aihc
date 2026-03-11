{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types responsible for CSV generation.
module Console.SolanaStaking.Csv
    ( makeCsvContents
    , ExportData (..)
    , toExportData
    ) where

import Data.Csv
    ( DefaultOrdered (..)
    , EncodeOptions (..)
    , ToNamedRecord (..)
    , defaultEncodeOptions
    , encodeDefaultOrderedByNameWith
    , namedRecord
    , (.=)
    )
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)

import Console.SolanaStaking.Api
    ( StakeReward (..)
    , StakingAccount (..)
    , StakingPubKey (..)
    , renderLamports
    )

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T


-- | Represents a single row of CSV data.
data ExportData = ExportData
    { edTime :: T.Text
    , edAmount :: T.Text
    , edStakeAccount :: T.Text
    , edEpoch :: Integer
    }
    deriving (Show, Read, Eq, Ord)


-- | Remove the @ed@ prefixes from the field names.
instance ToNamedRecord ExportData where
    toNamedRecord ExportData {..} =
        namedRecord
            [ "time" .= edTime
            , "amount" .= edAmount
            , "stakeAccount" .= edStakeAccount
            , "epoch" .= edEpoch
            ]


-- | Column order is @time, amount, stakeAccount, epoch@.
instance DefaultOrdered ExportData where
    headerOrder _ = ["time", "amount", "stakeAccount", "epoch"]


-- | Convert an Account & Reward into a CSV row.
toExportData :: (StakingAccount, StakeReward) -> ExportData
toExportData (StakingAccount {..}, StakeReward {..}) =
    ExportData
        { edTime = formatTimestamp srTimestamp
        , edAmount = renderLamports srAmount
        , edStakeAccount = fromStakingPubKey saPubKey
        , edEpoch = srEpoch
        }
  where
    formatTimestamp :: POSIXTime -> T.Text
    formatTimestamp =
        T.pack . formatTime defaultTimeLocale "%F %T%Z" . posixSecondsToUTCTime


-- | Build the CSV contents with a header row.
makeCsvContents :: [(StakingAccount, StakeReward)] -> LBS.ByteString
makeCsvContents =
    encodeDefaultOrderedByNameWith defaultEncodeOptions {encUseCrLf = False}
        . map toExportData
