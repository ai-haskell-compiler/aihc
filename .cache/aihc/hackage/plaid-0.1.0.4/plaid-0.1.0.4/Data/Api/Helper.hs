{-# LANGUAGE ScopedTypeVariables #-}

module Data.Api.Helper
  ( parseOrFail
  ) where


import           Control.Exception.Safe (MonadThrow, throwM)
import           Data.Aeson (FromJSON, eitherDecode)
import           Data.Api.Types
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

-- | Try to parse any plaid response to appropriate type or fail
parseOrFail
  :: forall b m.
     ( MonadThrow m
     , PlaidHttp m
     , FromJSON b)
  => BSL.ByteString
  -> m b
parseOrFail bs = do
  let edecoded = eitherDecode bs :: Either String b
  case edecoded of
    Left e        -> throwM (PlaidError $ T.pack e)
    Right decoded -> return decoded
