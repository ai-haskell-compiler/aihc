{-# OPTIONS_GHC -Wall #-}

module Control.Monad.RevState
   ( module Control.Monad.RevState.Class
   , module Control.Monad.Trans.RevState
   ) where

import Control.Monad.RevState.Class
import Control.Monad.Trans.RevState hiding
  ( get
  , put
  , state
  , modify
  , gets
  )
