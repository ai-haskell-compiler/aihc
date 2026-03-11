{-|
Module      : Control.Concurrent.Async.Refresh.Tokens.Conf
Description : Configuration related functions for the async-refresh-tokens package.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

module Control.Concurrent.Async.Refresh.Tokens.Conf
  ( TokenConf
  , defaultTokenConf
  , tokenConfSetFactor
  , tokenConfAddRequest
  ) where

import           Control.Concurrent.Async.Refresh.Tokens.Prelude

import qualified Control.Concurrent.Async.Refresh.Tokens.Lenses  as Lens
import           Control.Concurrent.Async.Refresh.Tokens.Types
import           Lens.Micro

-- | Produce default token configuration.
defaultTokenConf :: TokenConf m
defaultTokenConf =
  TokenConf { _tokenConfRefreshFactor = defaultRefreshTimeFactor
            , _tokenConfRequests      = [] }

-- | By default, we start refreshing tokens after 80% of the
-- "expires_in" time of a token has been elapsed.
defaultRefreshTimeFactor :: Double
defaultRefreshTimeFactor = 0.8

-- | Set the token refreshing factor, which is the factor in the
-- closed interval [0, 1] by which an "expires_in" duration is to be
-- scaled. See 'defaultRefreshTimeFactor'.
tokenConfSetFactor :: Double -> TokenConf m -> TokenConf m
tokenConfSetFactor = (Lens.refreshFactor .~)

-- | Add a token request to the given token configuration.
tokenConfAddRequest :: RequestToken m -> TokenConf m -> TokenConf m
tokenConfAddRequest req = Lens.requests %~ (req :)
