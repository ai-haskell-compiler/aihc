-- |
--
-- Module      :  Data.Plaid
-- Copyright   :  2019 Sasha Bogicevic
-- License     :  BSD 3 clause
--
-- Maintainer  :  Sasha Bogicevic <sasa.bogicevic@pm.me>
-- Stability   :  experimental
-- Portability :  portable
--
-- === Welcome to Plaid
-- As you could guess this library aims to provide easy
-- integration with Plaid.com

module Data.Plaid
  ( module Data.Api.Plaid
  , module Data.Api.Types
  ) where

import           Data.Api.Plaid
import           Data.Api.Types hiding (Plaid, PlaidTest, envUrl, requestMap)
