{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Codec.Compression.LZ4.Conduit as LZ4F
import           Control.Applicative (optional)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Options.Applicative (Parser, argument, str, metavar, long, short)
import qualified Options.Applicative as Opts
import           System.IO (stdin, stdout)


-- | Command line arguments of this program.
data CLIArgs = CLIArgs
  { m'inputFile :: Maybe Text
  , m'outputFile :: Maybe Text
  , decompress :: Bool
  } deriving (Eq, Ord, Show)


cliArgsParser :: Parser CLIArgs
cliArgsParser = CLIArgs
  <$> optional (T.pack <$> argument str (metavar "INPUT_FILE"))
  <*> optional (T.pack <$> argument str (metavar "OUTPUT-FILE"))
  <*> Opts.switch (long "decompress" <> short 'd')


-- | Parses the command line arguments for this program.
parseArgs :: IO CLIArgs
parseArgs = Opts.execParser $
  Opts.info
    (Opts.helper <*> cliArgsParser)
    (Opts.fullDesc <> Opts.progDesc "Compress or decompress .lz4 files")


main :: IO ()
main = do
  CLIArgs
    { m'inputFile
    , m'outputFile
    , decompress
    } <- parseArgs

  let source = case m'inputFile of
        Nothing -> CB.sourceHandle stdin
        Just "-" -> CB.sourceHandle stdin
        Just path -> CB.sourceFile (T.unpack path)

  let sink = case m'outputFile of
        Nothing -> CB.sinkHandle stdout
        Just "-" -> CB.sinkHandle stdout
        Just path -> CB.sinkFile (T.unpack path)

  let compressOrDecompress
        | decompress = LZ4F.decompress
        | otherwise  = LZ4F.compress

  runResourceT $ runConduit $ source .| compressOrDecompress .| sink
