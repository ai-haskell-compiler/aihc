{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Exception (bracket)
import           Codec (decode, encode, ForceFormat(..))
import           Data.Aeson (Result(Error, Success), Value, fromJSON)
import           Data.Aeson.Diff (patch)
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BSL
import           Options.Applicative (fullDesc, info, execParser, helper, metavar, progDesc, argument, help, value, long, option, short, switch)
import           Options.Applicative.Types (Parser, readerAsk)
import           System.IO (Handle, IOMode(ReadMode, WriteMode), hClose, openFile, stdin, stdout)

type File = Maybe FilePath

-- | Command-line options.
data PatchOptions = PatchOptions
    { optionOut   :: File -- ^ JSON destination
    , optionPatch :: File -- ^ Patch input
    , optionFrom  :: File -- ^ JSON source
    , optionYaml  :: Bool
    }

data Configuration = Configuration
    { cfgOptions :: PatchOptions
    , cfgOut   :: Handle
    , cfgPatch :: Handle
    , cfgFrom  :: Handle
    }

optionParser :: Parser PatchOptions
optionParser = PatchOptions
    <$> option fileP
        (  long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "Destination for patched JSON."
        <> value Nothing
        )
    <*> argument fileP
        (  metavar "PATCH"
        <> help "Patch to apply."
        )
    <*> argument fileP
        (  metavar "FROM"
        <> help "JSON file to patch."
        )
    <*> switch
        (  long "yaml"
        <> help "Use yaml decoding and encoding."
        )
  where
    fileP = do
        s <- readerAsk
        return $ case s of
            "-" -> Nothing
            _ -> Just s

jsonRead :: Handle -> ForceFormat -> File -> IO Value
jsonRead fp mformat mfilename = do
    s <- BS.hGetContents fp
    case decode mformat mfilename (BSL.fromStrict s) of
        Nothing -> error "Could not parse as JSON"
        Just v -> return v

run :: PatchOptions -> IO ()
run opt = bracket (load opt) close process
  where
    openr :: Maybe FilePath -> IO Handle
    openr Nothing = return stdin
    openr (Just p) = openFile p ReadMode

    openw :: Maybe FilePath -> IO Handle
    openw Nothing = return stdout
    openw (Just p) = openFile p WriteMode

    load :: PatchOptions -> IO Configuration
    load PatchOptions{..} =
        Configuration
            <$> pure opt
            <*> openw optionOut
            <*> openr optionPatch
            <*> openr optionFrom

    close :: Configuration -> IO ()
    close Configuration{..} = do
        hClose cfgPatch
        hClose cfgFrom
        hClose cfgOut

process :: Configuration -> IO ()
process Configuration{..} = do
    let mformat = if optionYaml cfgOptions then ForceYaml else AutodetectFormat
    json_patch <- jsonRead cfgPatch mformat (optionPatch cfgOptions)
    json_from <- jsonRead cfgFrom mformat (optionFrom cfgOptions)
    case fromJSON json_patch >>= flip patch json_from of
        Error e -> error e
        Success d -> BS.hPutStrLn cfgOut $ BSL.toStrict (encode mformat (optionOut cfgOptions) d)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> optionParser)
     (  fullDesc
     <> progDesc "Generate a patch between two JSON documents.")
