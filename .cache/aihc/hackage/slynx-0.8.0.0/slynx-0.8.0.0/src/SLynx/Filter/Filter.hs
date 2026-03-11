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
module SLynx.Filter.Filter
  ( filterRowsCmd,
    filterColsCmd,
  )
where

import Control.Monad (when)
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromMaybe)
import qualified ELynx.Sequence.Alignment as M
import ELynx.Sequence.Export.Fasta
import qualified ELynx.Sequence.Sequence as S
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import SLynx.Filter.Options
import SLynx.Tools

-- Chain a list of functions together. See https://wiki.haskell.org/Compose.
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

filterRows :: Maybe Int -> Maybe Int -> Bool -> [S.Sequence] -> BL.ByteString
filterRows ml ms std ss = sequencesToFasta $ compose filters ss
  where
    filters' =
      map (fromMaybe id) [S.filterLongerThan <$> ml, S.filterShorterThan <$> ms]
    filters = if std then S.filterStandard : filters' else filters'

-- | Filter sequences.
filterRowsCmd :: ELynx FilterRowsArguments ()
filterRowsCmd = do
  (FilterRowsArguments al inFile long short std) <- localArguments <$> ask
  maybe
    (return ())
    ( \val ->
        logInfoS $ "  Keep sequences longer than " <> show val <> "."
    )
    long
  maybe
    (return ())
    ( \val ->
        logInfoS $ "  Keep sequences shorter than " <> show val <> "."
    )
    short
  when std $
    logInfoS
      "  Keep sequences containing at least one standard (i.e., non-IUPAC) character."
  ss <- readSeqs al inFile
  let result = filterRows long short std ss
  out "filtered sequences" result ".fasta"

filterCols :: Maybe Double -> [S.Sequence] -> BL.ByteString
filterCols ms ss = sequencesToFasta . M.toSequences $ compose filters a
  where
    a = either error id (M.fromSequences ss)
    filters = map (fromMaybe id) [M.filterColsStd <$> ms]

-- | Filter columns.
filterColsCmd :: ELynx FilterColsArguments ()
filterColsCmd = do
  (FilterColsArguments al inFile standard) <- localArguments <$> ask
  case standard of
    Nothing -> return ()
    Just p ->
      logInfoS $
        "  Keep columns with a proportion of standard (non-IUPAC) characters larger than "
          ++ show p
          ++ "."
  ss <- readSeqs al inFile
  let result = filterCols standard ss
  out "filtered sequences" result ".fasta"
