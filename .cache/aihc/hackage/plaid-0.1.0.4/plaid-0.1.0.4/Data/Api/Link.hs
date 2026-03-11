-- |
--
-- Module      :  Data.Api.Link.Link
-- Copyright   :  2019 Sasha Bogicevic
-- License     :  BSD 3 clause
--
-- Maintainer  :  Sasha Bogicevic <sasa.bogicevic@pm.me>
-- Stability   :  experimental
-- Portability :  portable
--
module Data.Api.Link
  ( plaidCreatePublicToken
  , plaidExchangeToken
  ) where

import           Data.Api.Types (executePost)
import           Data.Common

-- | Creates a public token
-- that can be used in further interaction with plaid.com
-- https://plaid.com/docs/#creating-public-tokens
-- returns PlaidPublicTokenResponse or fails with PlaidError
plaidCreatePublicToken
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m
     )
  => PlaidBody PublicTokenCreate
  -> m PlaidPublicTokenResponse
plaidCreatePublicToken body = do
  env <- ask
  response <- executePost (envUrl (env ^. plaidEnvEnvironment) <> "/sandbox/public_token/create") body
  parseOrFail response

-- | Exchange a public token to access token.
-- Public token is invalidated after the exchange.
-- https://plaid.com/docs/#exchange-token-flow
-- returns PlaidAccessTokenResponse or fails with PlaidError
plaidExchangeToken
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m)
  => PlaidBody PlaidTokenExchange
  -> m PlaidAccessTokenResponse
plaidExchangeToken body = do
  env <- ask
  response <- executePost (envUrl (env ^. plaidEnvEnvironment) <> "/item/public_token/exchange") body
  parseOrFail response
