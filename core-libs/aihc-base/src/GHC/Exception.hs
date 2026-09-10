{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.Exception
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    ErrorCall (..),
    pattern ErrorCall,
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

import GHC.Base (String, (++))
import GHC.Exception.Type
  ( ArithException (..),
    Exception (..),
    SomeException (..),
    divZeroException,
    overflowException,
    ratioZeroDenomException,
    underflowException,
  )
import GHC.Internal.Stack (prettyCallStack, prettyCallStackLines)
import GHC.Prim (raise#)
import GHC.Prim.Show (Show (..), ShowS)
import GHC.Stack.Types (CallStack)
import GHC.Types (List (..))

throw :: (Exception e) => e -> a
throw exception = raise# (toException exception)

-- | The exception that 'GHC.Err.error' raises. The second field is the
-- rendered call stack of the call site, or the empty string when there is
-- none.
data ErrorCall = ErrorCallWithLocation String String

-- | An error call without a call stack. The pattern ignores the call stack
-- of an 'ErrorCallWithLocation', so it matches every 'ErrorCall'.
pattern ErrorCall :: String -> ErrorCall
pattern ErrorCall message <- ErrorCallWithLocation message _
  where
    ErrorCall message = ErrorCallWithLocation message []

{-# COMPLETE ErrorCall #-}

instance Show ErrorCall where
  showsPrec _ (ErrorCallWithLocation message []) = showString message
  showsPrec _ (ErrorCallWithLocation message location) =
    showString (message ++ ('\n' : location))

instance Exception ErrorCall

-- | The exception of an 'GHC.Err.errorWithoutStackTrace' call.
errorCallException :: String -> SomeException
errorCallException message = toException (ErrorCall message)

-- | The exception of an 'GHC.Err.error' call, which carries the call stack
-- of the call site.
errorCallWithCallStackException :: String -> CallStack -> SomeException
errorCallWithCallStackException message stack =
  toException (ErrorCallWithLocation message (prettyCallStack stack))

-- | 'GHC.Show.showString'. That module sits above this one, because it uses
-- 'GHC.Err.error'.
showString :: String -> ShowS
showString value suffix = value ++ suffix
