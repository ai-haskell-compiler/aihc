{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  TLynx.Simulate.Options
-- Description :  Argument parser for seq-ana
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri May  3 11:51:07 2019.
module TLynx.Simulate.Options
  ( Process (..),
    SimulateArguments (..),
    simulateArguments,
    reportSimulateArguments,
  )
where

import Data.Aeson
import Data.List
import Data.Maybe
import ELynx.Tools.Options
import ELynx.Tools.Reproduction
import ELynx.Tree.Simulate.PointProcess (TimeSpec (..))
import GHC.Generics
import Options.Applicative

deriving instance Eq TimeSpec

deriving instance Generic TimeSpec

instance Show TimeSpec where
  show Random = "Random"
  show (Origin o) = "Condition on height of origin: " ++ show o
  show (Mrca m) = "Condition on height of MRCA: " ++ show m

instance FromJSON TimeSpec

instance ToJSON TimeSpec

-- | Process to be used for simulation.
data Process
  = BirthDeath
      { -- | Birth rate.
        bdLambda :: Double,
        -- | Death rate.
        bdMu :: Double,
        -- | Sampling rate.
        bdRho :: Maybe Double,
        -- | Condition on height?
        bdHeight :: TimeSpec
      }
  | Coalescent
  deriving (Eq, Show, Generic)

instance FromJSON Process

instance ToJSON Process

reportProcess :: Process -> String
reportProcess (BirthDeath l m mr ts) =
  intercalate
    "\n"
    [ "Model: Birth and death process",
      "  Birth rate: " ++ show l,
      "  Death rate: " ++ show m,
      "  Sampling probability: " ++ maybe "1.0" show mr,
      "  Height specification: " ++ show ts
    ]
reportProcess Coalescent = "Model: Coalescent process"

-- | Arguments need to simulate phylogenetic trees using birth and death processes.
data SimulateArguments = SimulateArguments
  { -- | Simulated trees.
    argsNTrees :: Int,
    -- | Number of leaves.
    argsNLeaves :: Int,
    -- | Process.
    argsProcess :: Process,
    -- | Perform sub-sampling with given probability.
    argsSubSample :: Maybe Double,
    -- | Only print summary statistics?
    argsSumStat :: Bool,
    -- | Seed of NRG, random if 'Nothing'.
    argsSeed :: SeedOpt
  }
  deriving (Eq, Show, Generic)

instance Reproducible SimulateArguments where
  inFiles _ = []
  outSuffixes _ = [".tree"]
  getSeed = Just . argsSeed
  setSeed a s = a {argsSeed = s}
  parser = simulateArguments
  cmdName = "simulate"
  cmdDsc = ["Simulate phylogenetic trees using a birth and death or coalescent process."]
  cmdFtr = simulateFooter

instance FromJSON SimulateArguments

instance ToJSON SimulateArguments

-- | Print useful information about the provided arguments.
reportSimulateArguments :: SimulateArguments -> String
reportSimulateArguments a =
  intercalate
    "\n"
    [ "Number of simulated trees: " ++ show (argsNTrees a),
      "Number of leaves per tree: " ++ show (argsNLeaves a),
      reportProcess (argsProcess a),
      "Perform sub-sampling: " ++ ssStr,
      "Summary statistics only: " ++ show (argsSumStat a)
    ]
  where
    ssStr = case argsSubSample a of
      Nothing -> "No"
      Just p -> "Yes, with probability " ++ show p

-- | Command line parser.
simulateArguments :: Parser SimulateArguments
simulateArguments =
  SimulateArguments
    <$> nTreeOpt
    <*> nLeavesOpt
    <*> process
    <*> subSampleOpt
    <*> sumStatOpt
    <*> seedOpt

nTreeOpt :: Parser Int
nTreeOpt =
  option auto $
    long "nTrees"
      <> short 't'
      <> metavar "INT"
      <> help "Number of trees"

nLeavesOpt :: Parser Int
nLeavesOpt =
  option auto $
    long "nLeaves"
      <> short 'n'
      <> metavar "INT"
      <> help "Number of leaves per tree"

lambdaOpt :: Parser Double
lambdaOpt =
  option auto $
    long "lambda"
      <> short 'l'
      <> metavar "DOUBLE"
      <> help "Birth rate lambda"

muOpt :: Parser Double
muOpt =
  option auto $
    long "mu"
      <> short 'm'
      <> metavar "DOUBLE"
      <> help "Death rate mu"

rhoOpt :: Parser Double
rhoOpt =
  option auto $
    long "rho"
      <> short 'r'
      <> metavar "DOUBLE"
      <> help "Sampling probability rho"

mrca :: Parser TimeSpec
mrca =
  Mrca
    <$> option
      auto
      ( long "mrca"
          <> metavar "DOUBLE"
          <> help "Condition on height of most recent common ancestor"
      )

origin :: Parser TimeSpec
origin =
  Origin
    <$> option
      auto
      ( long "origin"
          <> metavar "DOUBLE"
          <> help "Condition on height of origin"
      )

timeSpec :: Parser TimeSpec
timeSpec = fromMaybe Random <$> optional (mrca <|> origin)

birthDeath :: Parser Process
birthDeath = BirthDeath <$> lambdaOpt <*> muOpt <*> optional rhoOpt <*> timeSpec

coalescent :: Parser Process
coalescent = pure Coalescent

process :: Parser Process
process =
  hsubparser $
    ( command
        "birthdeath"
        ( info
            birthDeath
            ( progDesc "Birth and death process"
                <> footer "Height: If no tree height is given, the heights will be randomly drawn from the expected distribution given the number of leaves, the birth and the death rate assuming a uniform prior."
            )
        )
        <> command
          "coalescent"
          (info coalescent (progDesc "Coalescent process"))
    )
      <> metavar "PROCESS"
      <> commandGroup "Available processes:"

subSampleOpt :: Parser (Maybe Double)
subSampleOpt =
  optional $
    option auto $
      long
        "sub-sample"
        <> short 'u'
        <> metavar "DOUBLE"
        <> showDefault
        <> help "Perform sub-sampling; see below."

sumStatOpt :: Parser Bool
sumStatOpt =
  switch $
    long "summary-statistics"
      <> short 's'
      <> showDefault
      <> help
        "For each branch, print length and number of children"

-- citation :: String
-- citation =
--   "Gernhard, T. (2008). The conditioned reconstructed process. Journal of Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005"

-- | And a footer.
simulateFooter :: [String]
simulateFooter =
  [ "See, for example, 'tlynx simulate birthdeath --help'.",
    "Sub-sample with probability p:\n  1. Simulate one big tree with n'=round(n/p), n'>=n, leaves;\n  2. Randomly sample sub trees with n leaves.\n  (With p=1.0, the same tree is reported over and over again.)"
  ]
