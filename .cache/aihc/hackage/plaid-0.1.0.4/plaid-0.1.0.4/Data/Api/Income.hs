module Data.Api.Income
  ( plaidGetIncome
  ) where

import           Data.Common

plaidGetIncome
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m
     )
  => PlaidBody PlaidIncomeGet
  -> m ByteString
plaidGetIncome body = do
  env <- ask
  let url = envUrl (env ^. plaidEnvEnvironment)
  executePost (url <> "/income/get") body
