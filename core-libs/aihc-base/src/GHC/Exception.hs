{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Exception
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    ErrorCall (.., ErrorCallWithLocation),
    divZeroException,
    overflowException,
    ratioZeroDenomException,
    underflowException,
    errorCallException,
    errorCallWithCallStackException,
    throw,
    prettyCallStackLines,
  )
where

import GHC.Exception.Type
  ( ArithException (..),
    Exception (..),
    SomeException (..),
    divZeroException,
    overflowException,
    ratioZeroDenomException,
    underflowException,
  )
import GHC.Internal.Base (String, (++))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.Exception.Context (ExceptionAnnotation (..))
import GHC.Internal.Exception.Type (addExceptionContext)
import GHC.Internal.Stack (prettyCallStackLines)
import GHC.Prim (raise#)
import GHC.Prim.Show (Show (..), ShowS)
import GHC.Stack.Types (CallStack)
import GHC.Types (List (..), Ordering (..))

throw :: (Exception e) => e -> a
throw exception = raise# (toException exception)

-- | The exception that 'GHC.Err.error' raises, as in GHC 9.12. The call
-- stack of the call site is not part of the value. 'error' puts it into the
-- exception context, where the top handler shows it.
data ErrorCall = ErrorCall String

instance Eq ErrorCall where
  ErrorCall left == ErrorCall right = left == right

-- | The order of the messages. The 'Ord' instance of lists is in the
-- "Prelude", which sits above this module.
instance Ord ErrorCall where
  compare (ErrorCall left) (ErrorCall right) = compareString left right

compareString :: String -> String -> Ordering
compareString [] [] = EQ
compareString [] _ = LT
compareString _ [] = GT
compareString (left : lefts) (right : rights) =
  case compare left right of
    EQ -> compareString lefts rights
    other -> other

-- | The old constructor with a location, as in GHC 9.12. A match gives an
-- error for the location, and a construction ignores the location.
pattern ErrorCallWithLocation :: String -> String -> ErrorCall
pattern ErrorCallWithLocation message location <- (errorCallWithLocationView -> (message, location))
  where
    ErrorCallWithLocation message _ = ErrorCall message

{-# COMPLETE ErrorCallWithLocation #-}

errorCallWithLocationView :: ErrorCall -> (String, String)
errorCallWithLocationView (ErrorCall message) =
  ( message,
    errorWithoutContext "ErrorCallWithLocation has been deprecated in favour of ErrorCall (which does not have a location). Backtraces are now handled by the backtrace exception mechanisms exclusively."
  )

-- | Raise an 'ErrorCall' without a call stack. "GHC.Err" sits above this
-- module.
errorWithoutContext :: String -> a
errorWithoutContext message = raise# (toException (ErrorCall message))

instance Show ErrorCall where
  showsPrec _ (ErrorCall message) = showString message

instance Exception ErrorCall

-- | The call stack of an 'GHC.Err.error' call, in the exception context.
newtype ErrorCallStack = ErrorCallStack CallStack

instance ExceptionAnnotation ErrorCallStack where
  displayExceptionAnnotation (ErrorCallStack stack) =
    case prettyCallStackLines stack of
      _ : frames -> unlinesWithoutLast ("HasCallStack backtrace:" : frames)
      [] -> "HasCallStack backtrace:"

unlinesWithoutLast :: [String] -> String
unlinesWithoutLast [] = []
unlinesWithoutLast [line] = line
unlinesWithoutLast (line : rest) = line ++ ('\n' : unlinesWithoutLast rest)

-- | The exception of an 'GHC.Err.errorWithoutStackTrace' call.
errorCallException :: String -> SomeException
errorCallException message = toException (ErrorCall message)

-- | The exception of an 'GHC.Err.error' call. The call stack of the call site
-- is in the exception context.
errorCallWithCallStackException :: String -> CallStack -> SomeException
errorCallWithCallStackException message stack =
  addExceptionContext (ErrorCallStack stack) (toException (ErrorCall message))

-- | 'GHC.Show.showString'. That module sits above this one, because it uses
-- 'GHC.Err.error'.
showString :: String -> ShowS
showString value suffix = value ++ suffix
