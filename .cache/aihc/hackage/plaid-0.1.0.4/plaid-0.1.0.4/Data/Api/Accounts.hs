-- |
--
-- Module      :  Data.Api.Product.Accounts
-- Copyright   :  2019 Sasha Bogicevic
-- License     :  BSD 3 clause
--
-- Maintainer  :  Sasha Bogicevic <sasa.bogicevic@pm.me>
-- Stability   :  experimental
-- Portability :  portable
--
module Data.Api.Accounts
  ( plaidGetBalance
  ) where

import           Data.Api.Types (executePost)
import           Data.Common

-- | Get the account balance
-- http://plaid.com/docs/#retrieve-balance-request
plaidGetBalance
  :: ( MonadReader PlaidEnv m
     , PlaidHttp m)
  => PlaidBody GetBalance
  -> m ByteString
plaidGetBalance body = do
  env <- ask
  executePost (envUrl (env ^. plaidEnvEnvironment) <> "/accounts/balance/get") body
