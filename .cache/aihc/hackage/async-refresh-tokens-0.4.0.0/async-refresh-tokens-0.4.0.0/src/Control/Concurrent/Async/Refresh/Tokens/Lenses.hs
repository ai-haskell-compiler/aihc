{-|
Module      : Control.Concurrent.Async.Refresh.Tokens.Lenses
Description : This module defines lenses used within the async-refresh-tokens package.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Control.Concurrent.Async.Refresh.Tokens.Lenses where

import           Control.Concurrent.Async.Refresh.Tokens.Types
import           Lens.Micro.TH

makeFields ''TokenConf
