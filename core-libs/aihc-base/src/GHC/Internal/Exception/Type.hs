{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.Internal.Exception.Type
  ( Exception (..),
    SomeException (SomeException),
    ExceptionWithContext (..),
    someExceptionContext,
  )
where

import Data.Typeable (Typeable, cast)
import GHC.Base (Maybe (..), String, (++))
import GHC.Internal.Exception.Context (ExceptionContext, emptyExceptionContext)
import GHC.Prim.Show (Show (..))

class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  toException = SomeException

  fromException :: SomeException -> Maybe e
  fromException (SomeException exception) = cast exception

  displayException :: e -> String
  displayException = show

data SomeException = forall e. (Exception e) => SomeExceptionWithContext ExceptionContext e

pattern SomeException :: () => (Exception e) => e -> SomeException
pattern SomeException exception <- SomeExceptionWithContext _ exception
  where
    SomeException exception = SomeExceptionWithContext emptyExceptionContext exception

{-# COMPLETE SomeException #-}

someExceptionContext :: SomeException -> ExceptionContext
someExceptionContext (SomeExceptionWithContext context _) = context

data ExceptionWithContext e = ExceptionWithContext ExceptionContext e

instance (Show e) => Show (ExceptionWithContext e) where
  showsPrec _ (ExceptionWithContext _ exception) suffix =
    "ExceptionWithContext _ " ++ show exception ++ suffix

instance (Exception e) => Exception (ExceptionWithContext e) where
  toException (ExceptionWithContext context exception) =
    case toException exception of
      SomeException value -> SomeExceptionWithContext context value
  fromException exception =
    case fromException exception of
      Just value -> Just (ExceptionWithContext (someExceptionContext exception) value)
      Nothing -> Nothing
  displayException (ExceptionWithContext _ exception) = displayException exception

instance Show SomeException where
  showsPrec precedence (SomeException exception) = showsPrec precedence exception

instance Exception SomeException where
  toException exception = exception
  fromException = Just
  displayException (SomeException exception) = displayException exception
