module Data.Api.Transactions
  ( plaidGetTransactions
  ) where

import           Data.Common

plaidGetTransactions
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m
     )
  => PlaidBody PlaidTransactionsGet
  -> m ByteString
plaidGetTransactions body = do
  env <- ask
  let url = envUrl (env ^. plaidEnvEnvironment)
  executePost (url <> "/transactions/get") body

