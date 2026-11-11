{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}

module GHC.Internal.Exception.Type
  ( Exception (..),
    SomeException (..),
    HasExceptionContext,
    ExceptionWithContext (..),
    someExceptionContext,
    addExceptionContext,
    mapExceptionContext,
  )
where

import Data.Typeable.Internal (Typeable, cast)
import GHC.Base (Maybe (..), String, (++))
import GHC.Internal.Exception.Context (ExceptionAnnotation, ExceptionContext, addExceptionAnnotation)
import GHC.Prim.Show (Show (..))

class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  toException = SomeException

  fromException :: SomeException -> Maybe e
  fromException (SomeException exception) = cast exception

  displayException :: e -> String
  displayException = show

-- | The context of the exception that a handler catches. An unsolved
-- constraint gets the empty context, as in GHC.
type HasExceptionContext = (?exceptionContext :: ExceptionContext)

data SomeException = forall e. (Exception e, HasExceptionContext) => SomeException e

someExceptionContext :: SomeException -> ExceptionContext
someExceptionContext (SomeException _) = ?exceptionContext

addExceptionContext :: (ExceptionAnnotation a) => a -> SomeException -> SomeException
addExceptionContext annotation = mapExceptionContext (addExceptionAnnotation annotation)

mapExceptionContext :: (ExceptionContext -> ExceptionContext) -> SomeException -> SomeException
mapExceptionContext function exception@(SomeException value) =
  let ?exceptionContext = function (someExceptionContext exception)
   in SomeException value

data ExceptionWithContext e = ExceptionWithContext ExceptionContext e

instance (Show e) => Show (ExceptionWithContext e) where
  showsPrec _ (ExceptionWithContext _ exception) suffix =
    "ExceptionWithContext _ " ++ show exception ++ suffix

instance (Exception e) => Exception (ExceptionWithContext e) where
  toException (ExceptionWithContext context exception) =
    case toException exception of
      SomeException value ->
        let ?exceptionContext = context
         in SomeException value
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
