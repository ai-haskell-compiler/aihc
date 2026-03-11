{-|
Module      : Control.Concurrent.Async.Refresh.Prelude
Description : Prelude for the async-refresh package.
Copyright   : (c) Moritz Schulte, 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

module Control.Concurrent.Async.Refresh.Prelude
  ( module Prelude
  , module Control.Exception.Safe
  , module Formatting
  , module Control.Monad.Logger
  , module UnliftIO.Async
  , threadDelay
  , Text
  , fromMaybe
  , forever, void
  , MonadIO
  , undefined
  , tshow
  ) where

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text
import           Formatting
import           Prelude                hiding (head, tail, undefined)
import qualified Prelude
import           UnliftIO.Async
import           UnliftIO.Concurrent

-- | Version of 'undefined' with a deprecated pragma.
undefined :: a
undefined = Prelude.undefined
{-# DEPRECATED undefined "Don't use 'undefined' in production code" #-}

-- | Modern version of 'Prelude.show' producing 'Text' instead of
-- 'String'.
tshow :: Show a => a -> Text
tshow = Data.Text.pack . show
