module Data.Proof.TransactionsGetBody
  ( HasTransactionsGetBody
  , proveTransactionsGetBody
  ) where

import           Data.Api.Types
import           Data.Proof.Named
import           Data.Text        (Text)
import           Data.Time        (Day)

data HasTransactionsGetBody plaidEnv accessToken startDate endDate options paginationOptions = Proof

proveTransactionsGetBody
  :: Named plaidEnv PlaidEnv
  -> Named accessToken AccessToken
  -> Named startDate Day
  -> Named endDate Day
  -> Named options (Maybe PlaidOptions)
  -> Named paginationOptions (Maybe PlaidPaginationOptions)
  -> Either Text (HasTransactionsGetBody plaidEnv accessToken startDate endDate options paginationOptions)
proveTransactionsGetBody _ atoken _ _ _ _ =
  case unWrapNamed atoken of
    (AccessToken "") -> Left "AccessToken cannot be empty"
    (AccessToken _ ) -> Right Proof
