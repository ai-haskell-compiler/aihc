module Data.Api.Auth
  ( plaidGetAuth
  ) where

import           Data.Api.Types (executePost)
import           Data.Common

plaidGetAuth
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m
     )
  => PlaidBody AuthGet
  -> m ByteString
plaidGetAuth body = do
  env <- ask
  executePost (envUrl (env ^. plaidEnvEnvironment) <> "/auth/get") body
