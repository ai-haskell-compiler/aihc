{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.
-}
module Console.Binance.Exports.Main
    ( run
    , getArgs
    , Args(..)
    , loadConfigFile
    , ConfigFile(..)
    ) where

import           Control.Applicative            ( (<|>) )
import           Control.Exception.Safe         ( try )
import           Control.Monad                  ( (<=<) )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( (.:?)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe )
import           Data.Ord                       ( Down(..) )
import           Data.Time                      ( UTCTime(..)
                                                , toGregorian
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.Version                   ( showVersion )
import           Data.Yaml                      ( prettyPrintParseException )
import           Data.Yaml.Config               ( ignoreEnv
                                                , loadYamlSettings
                                                )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , args
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

import           Console.Binance.Exports.Csv
import           Paths_binance_exports          ( version )
import           Web.Binance

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


-- | Generate & print a trade export based on the executable arguments.
run :: ConfigFile -> Args -> IO ()
run cfg cfgArgs = do
    AppConfig {..} <- mergeCfgEnvArgs cfg cfgArgs
    results        <- runApi binanceCfg $ do
        symbolDetails <-
            fmap eiSymbols $ getExchangeInfo symbols >>= handleBinanceError
        rawExportData <- concat <$> mapM getTradesForSymbol symbolDetails
        return . filterYear year $ sortOn (Down . tTime . tedTrade)
                                          rawExportData
    -- Write CSV to file or stdout
    let outputFileOrStdout = fromMaybe "-" outputFile
    let output             = buildTradeExport results
    if outputFileOrStdout == "-"
        then LBS.putStr output
        else LBS.writeFile outputFileOrStdout output
  where
    -- | If an error is present, print the code & message to stderr, then
    -- exit with an error status code.
    handleBinanceError :: Either BinanceError a -> BinanceApiM a
    handleBinanceError = \case
        Left e ->
            liftIO
                $  exitWithErr
                $  "Binance API Error Code "
                <> T.pack (show $ beCode e)
                <> ": "
                <> beMsg e
        Right a -> return a
    -- | Get all trades for the given symbol & convert them into the export
    -- format.
    getTradesForSymbol :: SymbolDetails -> BinanceApiM [TradeExportData]
    getTradesForSymbol s =
        map (TradeExportData s) <$> getTradeHistory (sdSymbol s) Nothing Nothing
    -- | Filter the trades if a 'year' argument has been passed.
    filterYear :: Maybe Integer -> [TradeExportData] -> [TradeExportData]
    filterYear = \case
        Nothing -> id
        Just y ->
            filter
                $ (\(y_, _, _) -> y == y_)
                . toGregorian
                . utctDay
                . posixSecondsToUTCTime
                . tTime
                . tedTrade

-- | Print some error text to 'stderr', then exit with a failure code.
exitWithErr :: T.Text -> IO a
exitWithErr = const exitFailure <=< T.hPutStrLn stderr . ("[ERROR] " <>)


-- CONFIGURATION

data AppConfig = AppConfig
    { binanceCfg :: BinanceConfig
    , symbols    :: [T.Text]
    , year       :: Maybe Integer
    , outputFile :: Maybe FilePath
    }
    deriving (Show, Eq)

-- | Given a parsed configuration file & CLI arguments, check for
-- environmental variables and either build an AppConfig or log an error
-- & exit if no API credentials or symbols have been passed.
mergeCfgEnvArgs :: ConfigFile -> Args -> IO AppConfig
mergeCfgEnvArgs ConfigFile {..} Args {..} = do
    envApiKey    <- fmap T.pack <$> lookupEnv "BINANCE_API_KEY"
    envApiSecret <- fmap T.pack <$> lookupEnv "BINANCE_API_SECRET"
    apiKey       <-
        requiredValue "Pass a Binance API Key with `-k` or $BINANCE_API_KEY."
        $   argApiKey
        <|> envApiKey
        <|> cfgApiKey
    apiSecret <-
        requiredValue
            "Pass a Binance API Secret with `-s` or $BINANCE_API_SECRET."
        $   argApiSecret
        <|> envApiSecret
        <|> cfgApiSecret
    let binanceCfg =
            BinanceConfig { bcApiKey = apiKey, bcApiSecret = apiSecret }
    symbols <- case (argSymbols, fromMaybe [] cfgSymbols) of
        ([], []) -> exitWithErr "Pass at least one symbol."
        ([], s ) -> return s
        (s , _ ) -> return s
    return AppConfig { year = argYear, outputFile = argOutputFile, .. }
  where
    requiredValue :: T.Text -> Maybe a -> IO a
    requiredValue errMsg = maybe (exitWithErr errMsg) return


-- CONFIG FILE

-- | Optional configuration data parsed from the config file.
data ConfigFile = ConfigFile
    { cfgApiKey    :: Maybe T.Text
    , cfgApiSecret :: Maybe T.Text
    , cfgSymbols   :: Maybe [T.Text]
    }
    deriving (Show, Eq)

instance FromJSON ConfigFile where
    parseJSON = withObject "ConfigFile" $ \o -> do
        cfgApiKey    <- o .:? "api-key"
        cfgApiSecret <- o .:? "api-secret"
        cfgSymbols   <- o .:? "symbols"
        return ConfigFile { .. }

-- | Attempt to read a 'ConfigFile' from
-- @$XDG_CONFIG_HOME\/binance-exports\/config.yaml@. Print any parsing
-- errors to 'stderr'.
loadConfigFile :: IO ConfigFile
loadConfigFile = do
    configPath   <- getUserConfigFile "binance-exports" "config.yaml"
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
    defaultConfig = ConfigFile Nothing Nothing Nothing


-- CLI ARGS

-- | CLI arguments supported by the executable.
data Args = Args
    { argApiKey     :: Maybe T.Text
    , argApiSecret  :: Maybe T.Text
    , argSymbols    :: [T.Text]
    , argYear       :: Maybe Integer
    , argOutputFile :: Maybe FilePath
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
                              &= help "Binance API Key"
                              &= name "k"
                              &= name "api-key"
                              &= explicit
                              &= typ "KEY"
            , argApiSecret  = def
                              &= help "Binance API Secret"
                              &= name "s"
                              &= name "api-secret"
                              &= explicit
                              &= typ "SECRET"
            , argYear       = Nothing
                              &= help "Limit output to year"
                              &= name "y"
                              &= name "year"
                              &= explicit
                              &= typ "YYYY"
            , argOutputFile =
                Nothing
                &= help "File to write the export to. Default: stdout"
                &= name "o"
                &= name "output-file"
                &= explicit
                &= typ "FILE"
            , argSymbols    = def &= args &= typ "SYMBOL [SYMBOL ...]"
            }
        &= summary
               (  "binance-exports v"
               <> showVersion version
               <> ", Pavan Rikhi 2022"
               )
        &= program "binance-exports"
        &= helpArg [name "h"]
        &= help "Export Binance Trade History to a CSV"
        &= details programDetails


programDetails :: [String]
programDetails = lines [r|
binance-exports generates a CSV export of your Binances Trade History. It
is intended to replace Binance's (removed) Trade History export.


DESCRIPTION

By default, we will pull every single trade you have made for the passed
symbols & print them out in reverse-chronological order with the following
fields:

   time,base-asset,quote-asset,type,price,quantity,total,fee,fee-currency,trade-id

This closely matches Binance's Trade History export, except we've split the
`symbol` column into `base-asset` & `quote-asset` columns and include the
`trade-id`.


OUTPUT FILE

You can use the `-o` flag to set the file we will write the CSV data into.
By default, the export is simply printed to stdout.

Warning: the export file will always be overwritten. We do not support
appending to an existing file.


ENVIRONMENTAL VARIABLES

Instead of passing in your API credentials via the `-k` & `-s` CLI flags,
you can set the `$BINANCE_API_KEY` & `$BINANCE_API_SECRET` environmental
variables.


CONFIGURATION FILE

You can also set some program options in a YAML file. We attempt to parse
a configuration file at `$XDG_CONFIG_HOME/binance-exports/config.yaml`. It
supports the following top-level keys:

    - `api-key`:        (string) Your Binance API key
    - `api-secret`:     (string) Your Binance API secret
    - `symbols`:        (list of strings) The trade symbols to fetch

Environmental variables will override any configuration options, and CLI
flags will override both environmental variables & configuration file
options.


USAGE EXAMPLES

Fetch all my BNB trades:
    binance-exports BNBUSD

Fetch my BTC trades from 2020:
    binance-exports -y 2020 BTCUSD

Fetch my BNB & BTC trades from 2022, write them to a file:
    binance-exports -y 2022 -o 2022-binance-trades.csv BNBUSD BTCUSD
|]
