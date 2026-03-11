-- |
-- Module      :  ELynx.Defaults
-- Description :  Various default values
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 23:00:17 2018.
module ELynx.Sequence.Defaults
  ( nameWidth,
    summaryLength,
    summaryNSequences,
    fieldWidth,
  )
where

-- | Space reserved for sequence names when printing them.
nameWidth :: Int
nameWidth = 23

-- | The length shown when summarizing sequences.
summaryLength :: Int
summaryLength = 60

-- | How many sequences are shown in summary.
summaryNSequences :: Int
summaryNSequences = 200

-- | Field width for tables.
fieldWidth :: Int
fieldWidth = 13
