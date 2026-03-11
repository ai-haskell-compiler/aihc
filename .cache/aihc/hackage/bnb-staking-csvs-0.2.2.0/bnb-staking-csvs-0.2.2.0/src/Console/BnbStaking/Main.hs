{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | CLI application harness.
module Console.BnbStaking.Main
    ( run
    , getArgs
    , Args (..)
    ) where

import Control.Monad (filterM, foldM, (<=<))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (localDay, toGregorian, utcToLocalZonedTime, zonedTimeToLocalTime)
import Data.Version (showVersion)
import Network.HTTP.Req (defaultHttpConfig, runReq)
import System.Console.CmdArgs
    ( Data
    , Typeable
    , argPos
    , cmdArgs
    , def
    , explicit
    , help
    , helpArg
    , name
    , program
    , summary
    , typ
    , (&=)
    )

import Console.BnbStaking.Api (Reward (..), getAllRewards)
import Console.BnbStaking.CoinTracking (makeCoinTrackingImport)
import Console.BnbStaking.Csv (makeCsvContents)
import Paths_bnb_staking_csvs (version)

import Data.ByteString.Lazy.Char8 qualified as LBC
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T


-- | Run the executable - parsing args, making queries, & printing the
-- results.
run :: Args -> IO ()
run Args {..} = do
    let aggregator = if argAggregate then aggregateRewards else return
        filterer = maybe return filterByYear argYear
    rewards <-
        filterer <=< aggregator <=< runReq defaultHttpConfig $
            getAllRewards (T.pack argPubKey)
    let outputFile = fromMaybe "-" argOutputFile
    if argCoinTracking
        then makeCoinTrackingImport outputFile argPubKey rewards
        else do
            -- TODO: we calculate local zoned day in filterByYear & in
            -- makeCsvContents. Can we do it just once?
            output <- makeCsvContents rewards
            if outputFile == "-"
                then LBC.putStr output
                else LBC.writeFile outputFile output
  where
    aggregateRewards :: [Reward] -> IO [Reward]
    aggregateRewards =
        fmap (mapMaybe (aggregate . snd) . M.toList)
            . foldM
                ( \acc r -> do
                    rewardTime <- utcToLocalZonedTime $ rRewardTime r
                    return $
                        M.insertWith
                            (<>)
                            (localDay $ zonedTimeToLocalTime rewardTime)
                            [r]
                            acc
                )
                M.empty
    aggregate :: [Reward] -> Maybe Reward
    aggregate = \case
        [] -> Nothing
        rs ->
            Just $
                Reward
                    { rValidatorName =
                        "AGGREGATE-"
                            <> T.intercalate
                                "-"
                                (L.sort $ map rValidatorName rs)
                    , rValidatorAddress = "AGGREGATE-" <> T.pack (show $ length rs)
                    , rDelegator = T.intercalate "-" $ L.sort $ map rDelegator rs
                    , rChainId = rChainId $ head rs
                    , rHeight = rHeight $ head rs
                    , rReward = sum $ map rReward rs
                    , rRewardTime = minimum $ map rRewardTime rs
                    }
    filterByYear :: Integer -> [Reward] -> IO [Reward]
    filterByYear year = filterM $ \reward -> do
        rewardTime <- utcToLocalZonedTime $ rRewardTime reward
        return
            . (\(y, _, _) -> y == year)
            . toGregorian
            . localDay
            $ zonedTimeToLocalTime rewardTime


-- | CLI arguments supported by the executable.
data Args = Args
    { argPubKey :: String
    -- ^ BinanceChain account's pubkey.
    , argOutputFile :: Maybe String
    -- ^ Optional output file. 'Nothing' or @'Just' "-"@ will print to
    -- 'System.IO.stdout'.
    , argCoinTracking :: Bool
    -- ^ Flag to enable writing/printing files formatted for CoinTracking
    -- Bulk Imports.
    , argYear :: Maybe Integer
    -- ^ Year for limiting the output.
    , argAggregate :: Bool
    -- ^ Aggregate rewards into day buckets.
    }
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


argSpec :: Args
argSpec =
    Args
        { argPubKey = def &= argPos 0 &= typ "ACCOUNT_PUBKEY"
        , argOutputFile =
            Nothing
                &= help "File to write the export to. Default: stdout"
                &= explicit
                &= name "output-file"
                &= name "o"
                &= typ "FILE"
        , argCoinTracking =
            False
                &= help "Generate a CoinTracking Import file."
                &= explicit
                &= name "cointracking"
        , argYear =
            Nothing
                &= help "Limit to given year"
                &= explicit
                &= name "y"
                &= name "year"
                &= typ "YYYY"
        , argAggregate =
            False
                &= help "Output one aggregate row per day."
                &= explicit
                &= name "aggregate"
        }
        &= summary
            ( "bnb-staking-csvs v"
                <> showVersion version
                <> ", Pavan Rikhi 2021-2024"
            )
        &= program "bnb-staking-csvs"
        &= helpArg [name "h"]
        &= help "Generate CSV Exports of you BinanceChain Staking Rewards."
