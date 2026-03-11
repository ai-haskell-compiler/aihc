module Data.Proof.IncomeGetBody
  ( HasIncomeGetBody
  , proveIncomeGetBody
  ) where

import           Data.Api.Types
import           Data.Proof.Named
import           Data.Text (Text)

data HasIncomeGetBody plaidEnv accessToken = Proof

-- | Check for creation of get income request body
proveIncomeGetBody
  :: Named plaidEnv PlaidEnv
  -> Named accessToken AccessToken
  -> Either Text (HasIncomeGetBody plaidEnv accessToken)
proveIncomeGetBody _ atoken =
  case unWrapNamed atoken of
    (AccessToken "") -> Left "AccessToken cannot be empty"
    (AccessToken _ ) -> Right Proof
