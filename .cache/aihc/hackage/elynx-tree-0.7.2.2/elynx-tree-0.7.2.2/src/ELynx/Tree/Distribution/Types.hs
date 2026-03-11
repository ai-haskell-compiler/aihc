-- |
-- Module      :  ELynx.Tree.Distribution.Types
-- Description :  Data types for distributions on trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed May 16 12:21:57 2018.
module ELynx.Tree.Distribution.Types
  ( Time,
    Rate,
  )
where

-- | Branch lengths are measured in time.
type Time = Double

-- | Birth or death rates.
type Rate = Double
