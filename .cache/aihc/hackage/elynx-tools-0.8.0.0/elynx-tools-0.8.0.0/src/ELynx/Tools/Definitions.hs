-- |
-- Module      :  ELynx.Tools.Definitions
-- Description :  Some global definitions
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Jan 25 17:17:41 2019.
module ELynx.Tools.Definitions
  ( -- * Definitions
    eps,
    precision,
  )
where

-- | Required precision when comparing 'Double' values.
eps :: Double
eps = 1e-12

-- | Default output precision.
precision :: Int
precision = 3
