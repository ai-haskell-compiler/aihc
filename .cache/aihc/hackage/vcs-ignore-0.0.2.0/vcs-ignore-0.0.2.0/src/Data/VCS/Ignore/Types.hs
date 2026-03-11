{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StrictData                #-}

{-|
Module      : Data.VCS.Ignore.Types
Description : Shared data types
Copyright   : (c) 2020-2022 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types and functions shared across the library.
-}

module Data.VCS.Ignore.Types
  ( VCSIgnoreError(..)
  , fromVCSIgnoreError
  , toVCSIgnoreError
  )
where

import           Control.Exception              ( Exception(..)
                                                , SomeException
                                                )
import           Data.Typeable                  ( cast )


---------------------------------  DATA TYPES  ---------------------------------

-- | Top-level of any exception thrown by this library.
data VCSIgnoreError = forall e . Exception e => VCSIgnoreError e

instance Show VCSIgnoreError where
  show (VCSIgnoreError e) = show e

instance Exception VCSIgnoreError where
  displayException (VCSIgnoreError e) = displayException e


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Unwraps given exception from 'VCSIgnoreError'.
fromVCSIgnoreError :: Exception e
                   => SomeException -- ^ exception to unwrap
                   -> Maybe e       -- ^ unwrapped exception
fromVCSIgnoreError e = do
  VCSIgnoreError e' <- fromException e
  cast e'


-- | Wraps given exception from 'VCSIgnoreError'.
toVCSIgnoreError :: Exception e
                 => e             -- ^ exception to wrap
                 -> SomeException -- ^ wrapped exception
toVCSIgnoreError = toException . VCSIgnoreError
