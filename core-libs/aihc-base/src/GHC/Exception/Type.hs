{-# LANGUAGE ExistentialQuantification #-}

module GHC.Exception.Type
  ( Exception (..),
    SomeException (..),
  )
where

import Data.Maybe (Maybe (..))
import Data.Typeable (Typeable, cast, tyConName, typeOf, typeRepTyCon)
import Prelude (String)

class (Typeable e) => Exception e where
  toException :: e -> SomeException
  toException = SomeException

  fromException :: SomeException -> Maybe e
  fromException (SomeException exception) = cast exception

  displayException :: e -> String
  displayException exception = tyConName (typeRepTyCon (typeOf exception))

data SomeException = forall e. (Exception e) => SomeException e

instance Exception SomeException where
  toException exception = exception
  fromException = Just
  displayException (SomeException exception) = displayException exception
