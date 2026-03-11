module Data.Api.Identity
  ( plaidGetIdentity
  ) where

import           Data.Common

plaidGetIdentity
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m
     )
  => PlaidBody PlaidIdentityGet
  -> m ByteString
plaidGetIdentity body = do
  env <- ask
  let url = envUrl (env ^. plaidEnvEnvironment)
  executePost (url <> "/identity/get") body
