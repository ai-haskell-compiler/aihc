{-|
Module      : Control.Concurrent.Async.Refresh.Tokens.Lenses
Description : This module defines the types used within the async-refresh-tokens package.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Control.Concurrent.Async.Refresh.Tokens.Types where

import           Control.Concurrent.Async.Refresh.Tokens.Prelude

import           Control.Concurrent.Async.Refresh
import           Data.Proxy
import           UnliftIO.Async
import           UnliftIO.STM

-- | Exceptions specific to this package.
data TokenException = TokenNotFound Text
 deriving (Typeable, Show)

instance Exception TokenException

-- | Type containing a token. 't' is a phantom type, which can be used
-- for making different tokens distinguishable at the type level.
newtype Token t = Token { unToken :: ByteString } deriving (Show, Eq)

-- | Type representing a configuration for this package.
data TokenConf m =
  TokenConf { _tokenConfRefreshFactor :: Double
            , _tokenConfRequests      :: [RequestToken m] }

-- | Type synonym representing a token store, which is an 'Either'
-- wrapped in a 'TVar'.
type TokenStore t = TVar (Either SomeException (Token t))

-- | Type class for tokens.
class IsToken t where
  -- | Desired scopes for this configuration.
  tokenScopes :: proxy t -> [Text]
  -- | Human readable name for this token, including default
  -- implementation.
  tokenName :: proxy t -> Text
  tokenName _ = tshow (Proxy :: Proxy t)

-- | Type wrapping a token request using existential quantification.
data RequestToken m = forall t. IsToken t
  => RequestToken
     { requestTokenStore  :: TokenStore t                -- ^ Token store to use.
     , requestTokenAction :: m (RefreshResult (Token t)) -- ^ Token refresh action.
     }

-- | Type representing a running token refresher.
newtype TokenRefresher = TokenRefresher (Async ())
