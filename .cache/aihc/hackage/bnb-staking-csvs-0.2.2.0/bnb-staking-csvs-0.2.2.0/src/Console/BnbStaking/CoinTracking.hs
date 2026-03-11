{-# LANGUAGE RecordWildCards #-}

-- | Generate & write/print CoinTracking Bulk Import files.
module Console.BnbStaking.CoinTracking
    ( makeCoinTrackingImport
    , writeOrPrintImportData
    , makeImportData
    , bnb
    ) where

import Control.Monad ((>=>))
import Data.Time (utcToLocalZonedTime)
import Web.CoinTracking.Imports
    ( Amount (..)
    , CTImportData (..)
    , CTTransactionType (Staking)
    , Currency (..)
    , coinTrackingCsvImport
    , writeImportDataToFile
    )

import Console.BnbStaking.Api (Reward (..))

import Data.ByteString.Lazy.Char8 qualified as LBC
import Data.Text qualified as T


-- | Generate the Bulk Import file for CoinTracking & write to destination
-- or print to stdout if destinatin is @"-"@.
makeCoinTrackingImport
    :: FilePath
    -- ^ Destination. @xls@ or @xlsx@ extensions generate the Excel import,
    -- any other extension will generate the CSV import.
    -> String
    -- ^ Account's PubKey
    -> [Reward]
    -> IO ()
makeCoinTrackingImport dest pubkey =
    makeImportData pubkey >=> writeOrPrintImportData dest


-- | Write or print the generated import data.
writeOrPrintImportData :: FilePath -> [CTImportData] -> IO ()
writeOrPrintImportData dest importData =
    if dest == "-"
        then LBC.putStrLn $ coinTrackingCsvImport importData
        else writeImportDataToFile dest importData


-- | Turn an account pubkey & reward into a 'CTImportData', localizing the
-- reward time.
makeImportData :: String -> [Reward] -> IO [CTImportData]
makeImportData pubkey = mapM $ \Reward {..} -> do
    zonedTime <- utcToLocalZonedTime rRewardTime
    return
        CTImportData
            { ctidType = Staking
            , ctidBuy = Just $ Amount rReward bnb
            , ctidSell = Nothing
            , ctidFee = Nothing
            , ctidExchange = "BNB Wallet"
            , ctidGroup = "Staking"
            , ctidComment = "Imported From bnb-staking-csvs"
            , ctidDate = zonedTime
            , ctidTradeId =
                "BNB-STAKE-"
                    <> T.pack pubkey
                    <> "-"
                    <> rValidatorAddress
                    <> "-"
                    <> T.pack (show rHeight)
            , ctidBuyValue = Nothing
            , ctidSellValue = Nothing
            }


-- | Binance Coin currency with the @BNB@ ticker & 8 decimals of precision.
bnb :: Currency
bnb = Currency 8 "BNB"
