-- |
-- Module      :  TLynx.Parsers
-- Description :  Parse Newick/Nexus tree files
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed Apr 22 13:34:35 2020.
module TLynx.Parsers
  ( parseTree,
    parseTrees,
    NewickFormat,
    newickFormat,
    newickHelp,
  )
where

import Data.List
import ELynx.Tools.InputOutput
import ELynx.Tree
import Options.Applicative

printError :: String -> String -> String -> IO a
printError fn new nex = do
  putStrLn $ "Error of Newick parser: " <> new <> "."
  putStrLn $ "Error of Nexus  parser: " <> nex <> "."
  error $ "Could not read tree file: " <> fn <> "."

-- | Parse a Newick tree file or a Nexus file with Newick trees.
--
-- Error if no or more than one trees are found.
-- Error if both file formats fail to parse.
parseTree :: NewickFormat -> FilePath -> IO (Tree Phylo Name)
parseTree fmt fn = do
  parseResultNewick <- runParserOnFile (oneNewick fmt) fn
  case parseResultNewick of
    Right r -> return r
    Left eNewick -> do
      parseResultNexus <- runParserOnFile (nexusTrees fmt) fn
      case parseResultNexus of
        Right [] -> error $ "No tree found in Nexus file " <> fn <> "."
        Right [(_, t)] -> return t
        Right _ -> error $ "More than one tree found in Nexus file " <> fn <> "."
        Left eNexus -> printError fn eNewick eNexus

-- | Parse a Newick tree file or a Nexus file with Newick trees.
--
-- Error if both file formats fail to parse.
parseTrees :: NewickFormat -> FilePath -> IO (Forest Phylo Name)
parseTrees fmt fn = do
  parseResultNewick <- runParserOnFile (someNewick fmt) fn
  case parseResultNewick of
    Right r -> return r
    Left eNewick -> do
      parseResultNexus <- runParserOnFile (nexusTrees fmt) fn
      case parseResultNexus of
        Right r -> return $ map snd r
        Left eNexus -> printError fn eNewick eNexus

-- | Parse 'NewickFormat'.
newickFormat :: Parser NewickFormat
newickFormat =
  option auto $
    long "newick-format"
      <> short 'f'
      <> metavar "FORMAT"
      <> value Standard
      <> help
        ( "Newick tree format: "
            ++ nwlist
            ++ "; default: Standard; for detailed help, see 'tlynx --help'"
        )
  where
    nwfs = map show ([minBound ..] :: [NewickFormat])
    nwlist = intercalate ", " (init nwfs) <> ", or " <> last nwfs

-- | Help for different 'NewickFormat's.
newickHelp :: [String]
newickHelp =
  map
    (toListItem . describeNewickFormat)
    ([minBound ..] :: [NewickFormat])
    ++ ["- Nexus file including Newick trees"]
  where
    toListItem = ("- Newick " ++)

-- Short description of the supported Newick formats.
describeNewickFormat :: NewickFormat -> String
describeNewickFormat Standard =
  "Standard: Branch support values are stored in square brackets after branch lengths."
describeNewickFormat IqTree =
  "IqTree:   Branch support values are stored as node names after the closing bracket of forests."
describeNewickFormat RevBayes =
  "RevBayes: Key-value pairs is provided in square brackets after node names as well as branch lengths. XXX: Key value pairs are ignored at the moment."
