{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Analyze.Analyze
-- Description :  Parse sequence file formats and analyze them
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 08:41:05 2018.
module SLynx.SubSample.SubSample
  ( subSampleCmd,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import qualified ELynx.Sequence.Alignment as M
import ELynx.Sequence.Export.Fasta
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import ELynx.Tools.Reproduction
import SLynx.SubSample.Options
import SLynx.Tools
import System.Random.Stateful

-- | Sub sample sequences.
subSampleCmd :: ELynx SubSampleArguments ()
subSampleCmd = do
  (SubSampleArguments al inFile nSites nAlignments sOpt) <- localArguments <$> ask
  let s = case fromSeedOpt sOpt of
        Nothing -> error "subSampleCmd: No seed."
        Just x -> x
  logInfoS $ "  Sample " <> show nSites <> " sites."
  logInfoS $ "  Sample " <> show nAlignments <> " multi sequence alignments."
  ss <- readSeqs al inFile
  gen <- newIOGenM $ mkStdGen s
  let a = either error id (M.fromSequences ss)
  samples <- liftIO $ replicateM nAlignments $ M.randomSubSample nSites a gen
  let results = map (sequencesToFasta . M.toSequences) samples
      sfxs = getOutSuffixes nAlignments "fasta"
  zipWithM_ (out "sub sampled multi sequence alignments") results sfxs
