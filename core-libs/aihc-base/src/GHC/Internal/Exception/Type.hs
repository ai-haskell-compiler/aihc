{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}

module GHC.Internal.Exception.Type
  ( Exception (..),
    SomeException (..),
    HasExceptionContext,
    ExceptionWithContext (..),
    NoBacktrace (..),
    someExceptionContext,
    addExceptionContext,
    mapExceptionContext,
  )
where

import Data.Typeable.Internal (Typeable, cast)
import GHC.Classes (Ord (..))
import GHC.Internal.Base (Maybe (..), String, otherwise, (++))
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

-- | An exception that does not want a backtrace when it is thrown. GHC
-- uses it to throw an exception again without a new backtrace. This
-- runtime collects no backtraces, so the wrapper only gives the exception
-- to its handlers.
newtype NoBacktrace e = NoBacktrace e

instance (Show e) => Show (NoBacktrace e) where
  showsPrec precedence (NoBacktrace exception) suffix
    | precedence >= 11 = '(' : body (')' : suffix)
    | otherwise = body suffix
    where
      body rest = "NoBacktrace " ++ showsPrec 11 exception rest

instance (Exception e) => Exception (NoBacktrace e) where
  toException (NoBacktrace exception) = toException exception
  fromException exception =
    case fromException exception of
      Just value -> Just (NoBacktrace value)
      Nothing -> Nothing

instance Show SomeException where
  showsPrec precedence (SomeException exception) = showsPrec precedence exception

instance Exception SomeException where
  toException exception = exception
  fromException = Just
  displayException (SomeException exception) = displayException exception
