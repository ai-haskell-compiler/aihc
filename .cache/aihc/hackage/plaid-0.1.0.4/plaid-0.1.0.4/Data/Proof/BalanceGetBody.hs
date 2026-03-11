module Data.Proof.BalanceGetBody
  ( HasGetBalanceBody
  , proveGetBalanceBody
  ) where

import           Data.Api.Types
import           Data.Proof.Named
import           Data.Text (Text)

data HasGetBalanceBody plaidEnv accessToken = Proof

-- | Check for creation of get balance request body
proveGetBalanceBody
  :: Named plaidEnv PlaidEnv
  -> Named accessToken AccessToken
  -> Either Text (HasGetBalanceBody plaidEnv accessToken)
proveGetBalanceBody _ atoken =
  case unWrapNamed atoken of
    (AccessToken "") -> Left "AccessToken cannot be empty"
    (AccessToken _ ) -> Right Proof
