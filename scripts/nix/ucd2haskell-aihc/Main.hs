{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Short qualified as BS
import GHC.Generics (Generic)
import UCD2Haskell.AIHC (genAihcModule)
import UCD2Haskell.ModuleGenerators (genModules)
import WithCli (HasArguments, withCli)

data CLIOptions = CLIOptions
  { input :: String,
    output :: String,
    core_prop :: [String]
  }
  deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient options = do
  genModules
    options.input
    options.output
    (BS.toShort . B8.pack <$> options.core_prop)
  genAihcModule options.input options.output

main :: IO ()
main = withCli cliClient
