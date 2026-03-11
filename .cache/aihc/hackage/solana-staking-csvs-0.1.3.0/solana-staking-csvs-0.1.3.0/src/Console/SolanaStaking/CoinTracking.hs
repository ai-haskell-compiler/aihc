{-# LANGUAGE RecordWildCards #-}

-- | Generate & write/print CoinTracking import files.
module Console.SolanaStaking.CoinTracking
    ( makeCoinTrackingImport
    , writeOrPrintImportData
    , makeImportData
    , sol
    ) where

import Control.Monad ((>=>))
import Data.Time (utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Web.CoinTracking.Imports
    ( Amount (..)
    , CTImportData (..)
    , CTTransactionType (Staking)
    , Currency (..)
    , coinTrackingCsvImport
    , writeImportDataToFile
    )

import Console.SolanaStaking.Api
    ( StakeReward (..)
    , StakingAccount (..)
    , StakingPubKey (..)
    , scientificLamports
    )

import Data.ByteString.Lazy.Char8 qualified as LBC
import Data.Text qualified as T


-- | Generate the Import file for CoinTracking & write to destination or
-- print to stdout.
makeCoinTrackingImport :: FilePath -> [(StakingAccount, StakeReward)] -> IO ()
makeCoinTrackingImport dest = makeImportData >=> writeOrPrintImportData dest


-- | Write or print the generated import data.
writeOrPrintImportData :: FilePath -> [CTImportData] -> IO ()
writeOrPrintImportData dest importData =
    if dest == "-"
        then LBC.putStrLn $ coinTrackingCsvImport importData
        else writeImportDataToFile dest importData


-- | Turn a 'StakeReward' into a 'CTImportData', localizing the reward
-- time.
makeImportData :: [(StakingAccount, StakeReward)] -> IO [CTImportData]
makeImportData = mapM $ \(StakingAccount {..}, StakeReward {..}) -> do
    zonedTime <- utcToLocalZonedTime $ posixSecondsToUTCTime srTimestamp
    return
        CTImportData
            { ctidType = Staking
            , ctidBuy = Just $ Amount (scientificLamports srAmount) sol
            , ctidSell = Nothing
            , ctidFee = Nothing
            , ctidExchange = "Solana Staking"
            , ctidGroup = "Staking"
            , ctidComment = "Imported from solana-staking-csvs"
            , ctidDate = zonedTime
            , ctidTradeId =
                "SOL-STAKE-"
                    <> fromStakingPubKey saPubKey
                    <> "-"
                    <> T.pack
                        (show srEpoch)
            , ctidBuyValue = Nothing
            , ctidSellValue = Nothing
            }


-- | @SOL@ currency with the @SOL2@ ticker & 9 decimals of precision.
sol :: Currency
sol = Currency 9 "SOL2"
