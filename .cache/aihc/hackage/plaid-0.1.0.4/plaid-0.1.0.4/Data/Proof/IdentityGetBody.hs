module Data.Proof.IdentityGetBody
  ( HasIdentityGetBody
  , proveIdentityGetBody
  ) where

import           Data.Api.Types
import           Data.Proof.Named
import           Data.Text (Text)

data HasIdentityGetBody plaidEnv accessToken = Proof

-- | Check for creation of get identity request body
proveIdentityGetBody
  :: Named plaidEnv PlaidEnv
  -> Named accessToken AccessToken
  -> Either Text (HasIdentityGetBody plaidEnv accessToken)
proveIdentityGetBody _ atoken =
  case unWrapNamed atoken of
    (AccessToken "") -> Left "AccessToken cannot be empty"
    (AccessToken _ ) -> Right Proof
