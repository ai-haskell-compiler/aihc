------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : queue-sheet program
-- Copyright   : Copyright (c) 2020-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- https://hackage.haskell.org/package/base
import Control.Applicative (optional)
import System.Exit (exitFailure)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- (queue-sheet)
import QueueSheet (version)
import QueueSheet.Build (buildPdf)

-- (queue-sheet:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Constants

-- | Default template file
defaultTemplate :: FilePath
defaultTemplate = "template.tex"

------------------------------------------------------------------------------
-- $Library

-- | Display an error and exit the program
errorExit :: String -> IO a
errorExit msg = do
    putStrLn $ "error: " ++ msg
    exitFailure

------------------------------------------------------------------------------
-- $Options

-- | Program options
data Options
  = Options
    { optTemplate :: !FilePath
    , optOutput   :: !(Maybe FilePath)
    , optQueues   :: !FilePath
    }
  deriving Show

-- | Parse program options
parseOptions :: IO Options
parseOptions = OA.execParser
    $ OA.info (LibOA.helper <*> LibOA.versioner version <*> options)
    $ mconcat
        [ OA.fullDesc
        , OA.progDesc "queue sheet utility"
        , OA.failureCode 2
        ]
  where
    options :: OA.Parser Options
    options = Options
      <$> templateOption
      <*> optional outputOption
      <*> queuesArgument

    templateOption :: OA.Parser FilePath
    templateOption = OA.strOption $ mconcat
      [ OA.long "template"
      , OA.short 't'
      , OA.metavar "TEMPLATE.tex"
      , OA.value defaultTemplate
      , OA.showDefaultWith id
      , OA.help "template file"
      ]

    outputOption :: OA.Parser FilePath
    outputOption = OA.strOption $ mconcat
      [ OA.long "output"
      , OA.short 'o'
      , OA.metavar "QUEUES.pdf"
      , OA.help "output file"
      ]

    queuesArgument :: OA.Parser FilePath
    queuesArgument = OA.strArgument $ mconcat
      [ OA.metavar "QUEUES.yaml"
      , OA.help "YAML file specifying queue information"
      ]

------------------------------------------------------------------------------
-- $main

main :: IO ()
main = do
  Options{..} <- parseOptions
  either errorExit return =<< buildPdf optQueues optTemplate optOutput
