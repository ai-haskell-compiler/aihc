{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the @main@ function used by the exectuable.
module Console.SolanaStaking.Main
    ( run
    , getArgs
    , Args (..)
    ) where

import Control.Monad (foldM, forM, unless, (<=<))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadIO, ReaderT, liftIO, runReaderT)
import Data.Bifunctor (bimap, second)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (LocalTime (..), ZonedTime (..), utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Version (showVersion)
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
import System.IO (hPutStrLn, stderr)

import Console.SolanaStaking.Api
import Console.SolanaStaking.CoinTracking (makeCoinTrackingImport)
import Console.SolanaStaking.Csv (makeCsvContents)
import Paths_solana_staking_csvs (version)

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as M
import Data.Text qualified as T


-- | Pull staking rewards data for the account & print a CSV to stdout.
--
-- TODO: concurrent/pooled requests, but w/ 100 req/s limit
run :: Args -> IO ()
run Args {..} = either (error . show) return <=< runner $ do
    -- Grab all stake accounts
    stakes <- saResults <$> (getAccountStakes >>= raiseAPIError)
    -- Grab rewards for each stake account
    (stakeErrors, stakeRewards) <-
        fmap (bimap concat concat . unzip) . forM stakes $ \sa -> do
            second (map (sa,))
                <$> maybe
                    (getAllStakeRewards (saPubKey sa))
                    (getYearsStakeRewards $ saPubKey sa)
                    argYear
    unless (null stakeErrors) . liftIO $ do
        hPutStrLn stderr "Got errors while fetching stake rewards:"
        mapM_ (hPutStrLn stderr . ("\t" <>) . show) stakeErrors
    -- Write the CSV
    let orderedRewards = sortOn (srTimestamp . snd) stakeRewards
        aggregator = if argAggregate then aggregateRewards else return
        outputFile = fromMaybe "-" argOutputFile
    rewards <- aggregator orderedRewards
    if argCoinTracking
        then liftIO $ makeCoinTrackingImport outputFile rewards
        else do
            let output = makeCsvContents rewards
            if outputFile == "-"
                then liftIO $ LBS.putStr output
                else liftIO $ LBS.writeFile outputFile output
  where
    runner :: ReaderT Config (ExceptT APIError IO) a -> IO (Either APIError a)
    runner = runExceptT . flip runReaderT (mkConfig argApiKey argPubKey)
    aggregateRewards :: (MonadIO m) => [(StakingAccount, StakeReward)] -> m [(StakingAccount, StakeReward)]
    aggregateRewards =
        fmap (mapMaybe (aggregate . snd) . M.toList)
            . foldM
                ( \acc r -> do
                    rewardTime <- liftIO . utcToLocalZonedTime $ posixSecondsToUTCTime $ srTimestamp $ snd r
                    return $
                        M.insertWith
                            (<>)
                            (localDay $ zonedTimeToLocalTime rewardTime)
                            [r]
                            acc
                )
                M.empty

    aggregate :: [(StakingAccount, StakeReward)] -> Maybe (StakingAccount, StakeReward)
    aggregate = \case
        [] -> Nothing
        rs ->
            Just
                ( StakingAccount
                    { saValidatorName = "AGGREGATE-" <> T.pack (show $ length rs)
                    , saPubKey = StakingPubKey $ T.pack argPubKey
                    , saLamports = sum $ map (saLamports . fst) rs
                    }
                , StakeReward
                    { srTimestamp = minimum $ map (srTimestamp . snd) rs
                    , srSlot = srSlot $ snd $ head rs
                    , srEpoch = srEpoch $ snd $ head rs
                    , srAmount = sum $ map (srAmount . snd) rs
                    }
                )


-- | CLI arguments supported by the executable.
data Args = Args
    { argApiKey :: String
    -- ^ Solana Beach API Key
    , argPubKey :: String
    -- ^ Delegator's PubKey.
    , argOutputFile :: Maybe String
    -- ^ Optional output file. 'Nothing' or @'Just' "-"@ means print to
    -- 'System.IO.stdout'.
    , argCoinTracking :: Bool
    -- ^ Flag to enable writing/printing files formatted for CoinTracking
    -- Imports.
    , argYear :: Maybe Integer
    -- ^ Year to limit output to.
    , argAggregate :: Bool
    -- ^ Aggregate rewards into single-day transactions.
    }
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


argSpec :: Args
argSpec =
    Args
        { argApiKey = def &= argPos 0 &= typ "API_KEY"
        , argPubKey = def &= argPos 1 &= typ "ACCOUNT_PUBKEY"
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
            ( "solana-staking-csvs v"
                <> showVersion version
                <> ", Pavan Rikhi 2021-2024"
            )
        &= program "solana-staking-csvs"
        &= helpArg [name "h"]
        &= help "Generate CSV Exports of your Solana Staking Rewards."
