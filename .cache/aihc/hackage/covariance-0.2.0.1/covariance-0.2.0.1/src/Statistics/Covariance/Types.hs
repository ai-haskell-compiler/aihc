-- |
-- Module      :  Statistics.Covariance.Types
-- Description :  Common types
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Sep 15 09:03:01 2021.
module Statistics.Covariance.Types
  ( DoCenter (..),
  )
where

-- | For some methods, data matrices have to be centered before estimation of
-- the covariance matrix. Sometimes, data matrices are already centered, and in
-- this case, duplicate centering can be avoided.
data DoCenter
  = -- | Perform centering.
    DoCenter
  | -- | Do not perform centering; assume the data matrix is already centered.
    AssumeCentered
