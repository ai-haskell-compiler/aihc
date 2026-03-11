{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.

-}
module Console.Gemini.Exports.Main
    ( run
    , getArgs
    , Args(..)
    , loadConfigFile
    , ConfigFile(..)
    ) where

import           Control.Applicative            ( (<|>) )
import           Control.Exception.Safe         ( try )
import           Control.Monad                  ( forM )
import           Data.Aeson                     ( (.:?)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( LocalTime(..)
                                                , UTCTime(..)
                                                , ZonedTime(..)
                                                , fromGregorian
                                                , getTimeZone
                                                , timeToTimeOfDay
                                                , zonedTimeToUTC
                                                )
import           Data.Version                   ( showVersion )
import           Data.Yaml                      ( prettyPrintParseException )
import           Data.Yaml.Config               ( ignoreEnv
                                                , loadYamlSettings
                                                )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , def
                                                , details
                                                , explicit
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )
import           System.Directory               ( doesFileExist )
import           System.Environment             ( lookupEnv )
import           System.Environment.XDG.BaseDir ( getUserConfigFile )
import           System.Exit                    ( exitFailure )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Text.RawString.QQ              ( r )

import           Console.Gemini.Exports.Csv
import           Paths_gemini_exports           ( version )
import           Web.Gemini

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T


-- | Run the executable.
run :: ConfigFile -> Args -> IO ()
run cfg cfgArgs = do
    AppConfig {..} <- makeConfig cfg cfgArgs
    exportData     <- runApi geminiCfg $ do
        trades <- getMyTrades dateRange
        let symbols = L.nub $ map tSymbol trades
        symbolDetails <- fmap M.fromList . forM symbols $ \symbol -> do
            (symbol, ) <$> getSymbolDetails symbol
        tradeExport <- fmap catMaybes . forM trades $ \t -> do
            let mbTrade = TradeExport t <$> M.lookup (tSymbol t) symbolDetails
            mapM makeExportData mbTrade
        transfers        <- getMyTransfers dateRange
        transferExport   <- mapM (makeExportData . TransferExport) transfers
        earnTransactions <- getMyEarnTransactions dateRange
        earnExport       <- mapM (makeExportData . EarnExport) earnTransactions
        return $ tradeExport <> transferExport <> earnExport
    let
        csvData = makeExportCsv
            $ L.sortOn (getExportLineTimestamp . edLine) exportData
    if outputFile == "-"
        then LBS.putStrLn csvData
        else LBS.writeFile outputFile csvData

-- | Print some text to stderr and then exit with an error.
exitWithError :: String -> IO a
exitWithError msg = hPutStrLn stderr ("[ERROR] " <> msg) >> exitFailure


-- CONFIGURATION

data AppConfig = AppConfig
    { geminiCfg  :: GeminiConfig
    , outputFile :: FilePath
    , dateRange  :: Maybe (UTCTime, UTCTime)
    }
    deriving (Show, Read, Eq, Ord)

-- | Pull Environmental variables, then merge the config file, env vars,
-- and cli args into an AppConfig.
--
-- Exit with an error if we cannot construct a 'GeminiConfig'.
makeConfig :: ConfigFile -> Args -> IO AppConfig
makeConfig ConfigFile {..} Args {..} = do
    envApiKey <- fmap T.pack <$> lookupEnv "GEMINI_API_KEY"
    gcApiKey  <-
        errorIfNothing "Pass a Gemini API Key with `-k` or $GEMINI_API_KEY."
        $   argApiKey
        <|> envApiKey
        <|> cfgApiKey
    envApiSecret <- fmap T.pack <$> lookupEnv "GEMINI_API_SECRET"
    gcApiSecret  <-
        errorIfNothing
            "Pass a Gemini API Secret with `-s` or $GEMINI_API_SECRET."
        $   argApiSecret
        <|> envApiSecret
        <|> cfgApiSecret
    let geminiCfg = GeminiConfig { .. }
    dateRange <- mapM buildDateRange argYear
    return AppConfig { outputFile = fromMaybe "-" argOutputFile, .. }
  where
    -- | Exit with error message if value is 'Nothing'
    errorIfNothing :: String -> Maybe a -> IO a
    errorIfNothing msg = maybe (exitWithError msg) return
    -- | Given a year, build a tuple representing the span of a year in the
    -- user's timezone.
    buildDateRange :: Integer -> IO (UTCTime, UTCTime)
    buildDateRange y = do
        let yearStart = UTCTime (fromGregorian y 1 1) 0
            yearEnd   = UTCTime (fromGregorian y 12 31)
                                ((23 * 60 * 60) + (59 * 60) + 59 + 0.9999)
        (,) <$> mkZonedTime yearStart <*> mkZonedTime yearEnd
    -- | Shift a time by the user's timezone - coercing it into a ZonedTime
    -- and converting that back into UTC.
    mkZonedTime :: UTCTime -> IO UTCTime
    mkZonedTime t = do
        tz <- getTimeZone t
        let localTime = LocalTime (utctDay t) (timeToTimeOfDay $ utctDayTime t)
            zonedTime = ZonedTime localTime tz
        return $ zonedTimeToUTC zonedTime


-- CONFIG FILE

-- | Optional configuration data parsed from a yaml file.
data ConfigFile = ConfigFile
    { cfgApiKey    :: Maybe Text
    , cfgApiSecret :: Maybe Text
    }
    deriving (Show, Read, Eq, Ord)

instance FromJSON ConfigFile where
    parseJSON = withObject "ConfigFile" $ \o -> do
        cfgApiKey    <- o .:? "api-key"
        cfgApiSecret <- o .:? "api-secret"
        return ConfigFile { .. }

-- | Attempt to read a 'ConfigFile' from
-- @$XDG_CONFIG_HOME\/gemini-exports\/config.yaml@. Print any parsing
-- errors to 'stderr'.
loadConfigFile :: IO ConfigFile
loadConfigFile = do
    configPath   <- getUserConfigFile "gemini-exports" "config.yaml"
    configExists <- doesFileExist configPath
    if configExists
        then try (loadYamlSettings [configPath] [] ignoreEnv) >>= \case
            Left (lines . prettyPrintParseException -> errorMsgs) ->
                hPutStrLn stderr "[WARN] Invalid Configuration Format:"
                    >> mapM_ (hPutStrLn stderr . ("\t" <>)) errorMsgs
                    >> return defaultConfig
            Right cfg -> return cfg
        else return defaultConfig
  where
    defaultConfig :: ConfigFile
    defaultConfig = ConfigFile Nothing Nothing


-- CLI ARGS

-- | CLI arguments supported by the executable.
data Args = Args
    { argApiKey     :: Maybe Text
    , argApiSecret  :: Maybe Text
    , argOutputFile :: Maybe FilePath
    , argYear       :: Maybe Integer
    }
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


-- | Defines & documents the CLI arguments.
argSpec :: Args
argSpec =
    Args
            { argApiKey     = def
                              &= name "api-key"
                              &= name "k"
                              &= explicit
                              &= help "Gemini API Key"
                              &= typ "KEY"
            , argApiSecret  = def
                              &= name "api-secret"
                              &= name "s"
                              &= explicit
                              &= help "Gemini API Secret"
                              &= typ "SECRET"
            , argOutputFile = Nothing
                              &= help "File to write export to. Default: stdout"
                              &= name "o"
                              &= name "output-file"
                              &= explicit
                              &= typ "FILE"
            , argYear       = Nothing
                              &= help "Limit transactions to given year."
                              &= name "y"
                              &= name "year"
                              &= explicit
                              &= typ "YYYY"
            }
        &= summary
               (  "gemini-exports v"
               <> showVersion version
               <> ", Pavan Rikhi 2022"
               )
        &= program "gemini-exports"
        &= helpArg [name "h"]
        &= help "Generate CSV Exports of your Gemini Trades."
        &= details programDetails


programDetails :: [String]
programDetails = lines [r|
gemini-exports generates a CSV export of your Gemini Trades, Earn
Transactions, & Transfers.


DESCRIPTION

By default, we will pull every single trade, transfer, and income you have
made on Gemini & print them out in chronological order with the following
fields:

   time,base-asset,quote-asset,type,description,price,quantity,total,fee,fee-currency,trade-id

Trades have blank descriptions.

Transfers have blank quote-assets, prices, & fees and potential blank
descriptions.

Earn transactions have blank descriptions & fees and potentially blank
quote-assets, price, & totals.


OUTPUT FILE

You can use the `-o` flag to set the file we will write the CSV data into.
By default, the export is simply printed to stdout.

Warning: the export file will always be overwritten. We do not support
appending to an existing file.


ENVIRONMENTAL VARIABLES

Instead of passing in your API credentials via the `-k` & `-s` CLI flags,
you can set the `$GEMINI_API_KEY` & `$GEMINI_API_SECRET` environmental
variables.


CONFIGURATION FILE

You can also set some program options in a YAML file. We attempt to parse
a configuration file at `$XDG_CONFIG_HOME/gemini-exports/config.yaml`. It
supports the following top-level keys:

    - `api-key`:        (string) Your Gemini API key
    - `api-secret`:     (string) Your Gemini API secret

Environmental variables will override any configuration options, and CLI
flags will override both environmental variables & configuration file
options.


USAGE EXAMPLES

Fetch all my trades, deposits, withdrawals, & earn transactions:
    gemini-exports -k <API_KEY> -s <API_SECRET>

Fetch my Gemini history from 2020:
    gemini-exports -y 2020

Fetch my history from 2022, write them to a file:
    gemini-exports -y 2022 -o 2022-gemini-history.csv
|]
