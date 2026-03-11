module Data.Proof.AuthGetBody
  ( HasAuthGetBody
  , proveAuthGetBody
  ) where

import           Data.Api.Types
import           Data.Proof.Named
import           Data.Text (Text)

data HasAuthGetBody plaidEnv accessToken = Proof

proveAuthGetBody
  :: Named plaidEnv PlaidEnv
  -> Named accessToken AccessToken
  -> Either Text (HasAuthGetBody plaidEnv accessToken)
proveAuthGetBody _ atoken =
  case unWrapNamed atoken of
    (AccessToken "") -> Left "AccessToken cannot be empty"
    (AccessToken _ ) -> Right Proof
