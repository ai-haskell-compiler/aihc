{-
 - This example is similar to the commandline iconv program.
 - Author: Conrad Parker, July 2007

  Usage: hiconv [options] filename

    -h, -?       --help, --usage       Display this help and exit
    -f encoding  --from-code=encoding  Convert characters from encoding
    -t encoding  --to-code=encoding    Convert characters to encoding
    -c           --discard             Discard invalid characters from output
                 --transliterate       Transliterate unconvertable characters
    -o file      --output=file         Specify output file (instead of stdout)

 -}

module Main where

import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt (getOpt, usageInfo,
                              OptDescr(..), ArgDescr(..), ArgOrder(..))
import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy as Lazy
import qualified Codec.Text.IConv as IConv

------------------------------------------------------------
-- main
--

main :: IO ()
main = do
    args <- getArgs
    (config, filenames) <- processArgs args

    let inputFile = head filenames
    input <- case inputFile of
        "-" -> Lazy.getContents
        _   -> Lazy.readFile inputFile

    let convert = case fuzzyConvert config of
                    Nothing   -> IConv.convert
                    Just fuzz -> IConv.convertFuzzy fuzz
        output = convert (fromEncoding config) (toEncoding config) input
        o = outputFile config

    case o of
        "-" -> Lazy.putStr output
        _   -> Lazy.writeFile o output

------------------------------------------------------------
-- Option handling
--

data Config =
    Config {
        fromEncoding :: String,
        toEncoding :: String,
	fuzzyConvert :: Maybe IConv.Fuzzy,
        outputFile :: FilePath
    }

defaultConfig =
    Config {
        fromEncoding = "",
        toEncoding = "",
	fuzzyConvert = Nothing,
        outputFile = "-"
    }

data Option = Help
            | FromEncoding String
            | ToEncoding String
	    | Discard | Translit
            | OutputFile String
            deriving Eq

options :: [OptDescr Option]
options = [ Option ['h', '?'] ["help", "usage"] (NoArg Help)
              "Display this help and exit"
          , Option ['f'] ["from-code"] (ReqArg FromEncoding "encoding")
              "Convert characters from encoding"
          , Option ['t'] ["to-code"] (ReqArg ToEncoding "encoding")
              "Convert characters to encoding"
	  , Option ['c'] ["discard"]       (NoArg Discard)
	      "Discard invalid characters from output"
	  , Option []    ["transliterate"] (NoArg Translit)
	      "Transliterate unconvertable characters"
          , Option ['o'] ["output"] (ReqArg OutputFile "file")
              "Specify output file (instead of stdout)"
          ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
    case getOpt Permute options args of
        (opts, args, errs) -> do
            processHelp opts
            let config = processConfig defaultConfig opts
            checkConfig errs config args
            return (config, args)

checkConfig :: [String] -> Config -> [String] -> IO ()
checkConfig errs config filenames = do
    when (any null [fromEncoding config, toEncoding config] || null filenames) $
      processHelp [Help]
    when (not (null errs)) $ do
      mapM_ putStr errs
      processHelp [Help]

processHelp :: [Option] -> IO ()
processHelp opts = do
    name <- getProgName
    let header = "\nUsage: " ++ name ++ " [options] filename\n"
    when (Help `elem` opts) $ do
        putStrLn $ usageInfo header options
        exitFailure

processConfig :: Config -> [Option] -> Config
processConfig = foldl processOneOption
    where
        processOneOption config (FromEncoding f) =
            config {fromEncoding = f}
        processOneOption config (ToEncoding t) =
            config {toEncoding = t}
        processOneOption config (OutputFile o) =
            config {outputFile = o}
        processOneOption config Discard =
	    config {fuzzyConvert = Just IConv.Discard}
        processOneOption config Translit =
            config {fuzzyConvert = Just IConv.Transliterate}
