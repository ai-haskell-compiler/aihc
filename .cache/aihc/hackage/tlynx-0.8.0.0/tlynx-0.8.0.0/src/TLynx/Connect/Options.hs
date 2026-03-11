{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  TLynx.Connect.Options
-- Description :  Options for the connect subcommand
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Sep 19 15:02:21 2019.
module TLynx.Connect.Options
  ( ConnectArguments (..),
    connectArguments,
  )
where

import Data.Aeson
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import TLynx.Parsers

-- | Arguments of connect command.
data ConnectArguments = ConnectArguments
  { nwFormat :: NewickFormat,
    constraints :: Maybe FilePath,
    inFileA :: FilePath,
    inFileB :: FilePath
  }
  deriving (Eq, Show, Generic)

instance Reproducible ConnectArguments where
  inFiles a = [inFileA a, inFileB a]
  outSuffixes _ = [".out"]
  getSeed _ = Nothing
  setSeed a _ = a
  parser = connectArguments
  cmdName = "connect"
  cmdDsc =
    [ "Connect two phylogenetic trees in all ways (possibly honoring constraints)."
    ]

instance FromJSON ConnectArguments

instance ToJSON ConnectArguments

-- | Parse arguments of connect command.
connectArguments :: Parser ConnectArguments
connectArguments =
  ConnectArguments <$> newickFormat <*> constraintsFile <*> fileA <*> fileB

constraintsFile :: Parser (Maybe FilePath)
constraintsFile =
  optional $
    strOption $
      metavar "CONSTRAINTS"
        <> short 'c'
        <> long "contraints"
        <> help "File containing one or more Newick trees to be used as constraints"

fileA :: Parser FilePath
fileA =
  strArgument $
    metavar "TREE-FILE-A"
      <> help
        "File containing the first Newick tree"

fileB :: Parser FilePath
fileB =
  strArgument $
    metavar "TREE-FILE-B"
      <> help
        "File containing the second Newick tree"
