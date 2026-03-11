{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  TLynx.Shuffle.Shuffle
-- Description :  Shuffle a phylogeny
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Sep 19 15:01:52 2019.
--
-- The coalescent times are unaffected. The topology and the leaf order is
-- shuffled. Branch support values are ignored and lost.
module TLynx.Shuffle.Shuffle
  ( shuffleCmd,
  )
where

import qualified Control.Comonad as C
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy.Char8 as BL
import ELynx.Tools.Definitions
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import ELynx.Tools.Reproduction
import ELynx.Tree
import ELynx.Tree.Simulate.PointProcess
  ( PointProcess (PointProcess),
    toReconstructedTree,
  )
import System.IO (hClose)
import System.Random.Stateful
import TLynx.Grabble
import TLynx.Parsers
import TLynx.Shuffle.Options

-- | Shuffle a tree. Get all coalescent times, shuffle them. Get all leaves,
-- shuffle them. Connect the shuffled leaves with the shuffled coalescent times.
-- The shuffled tree has a new topology while keeping the same set of coalescent
-- times and leaves.
shuffleCmd :: ELynx ShuffleArguments ()
shuffleCmd = do
  l <- localArguments <$> ask
  h <- outHandle "results" ".tree"
  let nwF = nwFormat l
  tPhylo <- liftIO $ parseTree nwF (inFile l)
  logInfoS "Input tree:"
  logInfoB $ toNewick tPhylo
  let t = either error id $ toLengthTree tPhylo
  -- Check if tree is ultrametric enough.
  let dh = sum $ map (height t -) (distancesOriginLeaves t)
  logDebugS $ "Distance in branch length to being ultrametric: " <> show dh
  when (dh > 2e-4) (error "Tree is not ultrametric.")
  when (dh > toLengthUnsafe eps && dh < 2e-4) $
    logInfoS
      "Tree is nearly ultrametric, ignore branch length differences smaller than 2e-4."
  when (dh < toLengthUnsafe eps) $ logInfoS "Tree is ultrametric."
  let cs = filter (> 0) $ labels $ C.extend rootHeight t
      ls = map getName $ leaves t
  logDebugS $ "Number of coalescent times: " <> show (length cs)
  logDebugS $ "Number of leaves: " <> show (length ls)
  logDebugS "The coalescent times are: "
  logDebugS $ show cs
  gen <- newIOGenM $ mkStdGen $ case argsSeed l of
    RandomUnset -> error "Seed not available; please contact maintainer."
    RandomSet s -> s
    Fixed s -> s
  ts <- liftIO $ shuffleT (nReplicates l) (height t) cs ls gen
  liftIO $ BL.hPutStr h $ BL.unlines $ map (toNewick . lengthToPhyloTree) ts
  liftIO $ hClose h

shuffleT ::
  (StatefulGen g m) =>
  Int -> -- How many?
  Length -> -- Stem length.
  [Length] -> -- Coalescent times.
  [Name] -> -- Leave names.
  g ->
  m (Forest Length Name)
shuffleT n o cs ls gen = do
  css <- grabble cs n (length cs) gen
  lss <- grabble ls n (length ls) gen
  return
    [ toReconstructedTree "" (PointProcess names times o)
      | (times, names) <- zip css lss
    ]
