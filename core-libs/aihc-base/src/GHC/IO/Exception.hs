{-# LANGUAGE ExistentialQuantification #-}

-- | The IO exception types. The IO error type lives in
-- "GHC.Internal.IO.Types".
module GHC.IO.Exception
  ( BlockedIndefinitelyOnMVar (..),
    blockedIndefinitelyOnMVar,
    BlockedIndefinitelyOnSTM (..),
    blockedIndefinitelyOnSTM,
    Deadlock (..),
    AllocationLimitExceeded (..),
    allocationLimitExceeded,
    AssertionFailed (..),
    SomeAsyncException (..),
    asyncExceptionToException,
    asyncExceptionFromException,
    AsyncException (..),
    stackOverflow,
    heapOverflow,
    ArrayException (..),
    IOException (..),
    IOError,
    IOErrorType (..),
    ioException,
    ioError,
    userError,
    unsupportedOperation,
  )
where

import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import GHC.Base (String, (.))
import GHC.Exception.Type (Exception (..), SomeException)
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types
  ( IOErrorType (..),
    IOException (..),
    ioError,
    ioException,
    unsupportedOperation,
    userError,
  )
import GHC.Show (Show (..), showString)

type IOError = IOException

data BlockedIndefinitelyOnMVar = BlockedIndefinitelyOnMVar

instance Exception BlockedIndefinitelyOnMVar

instance Show BlockedIndefinitelyOnMVar where
  showsPrec _ BlockedIndefinitelyOnMVar = showString "thread blocked indefinitely in an MVar operation"

blockedIndefinitelyOnMVar :: SomeException
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

data BlockedIndefinitelyOnSTM = BlockedIndefinitelyOnSTM

instance Exception BlockedIndefinitelyOnSTM

instance Show BlockedIndefinitelyOnSTM where
  showsPrec _ BlockedIndefinitelyOnSTM = showString "thread blocked indefinitely in an STM transaction"

blockedIndefinitelyOnSTM :: SomeException
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM

data Deadlock = Deadlock

instance Exception Deadlock

instance Show Deadlock where
  showsPrec _ Deadlock = showString "<<deadlock>>"

data AllocationLimitExceeded = AllocationLimitExceeded

instance Exception AllocationLimitExceeded where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

instance Show AllocationLimitExceeded where
  showsPrec _ AllocationLimitExceeded = showString "allocation limit exceeded"

allocationLimitExceeded :: SomeException
allocationLimitExceeded = toException AllocationLimitExceeded

newtype AssertionFailed = AssertionFailed String

instance Exception AssertionFailed

instance Show AssertionFailed where
  showsPrec _ (AssertionFailed message) = showString message

-- | The parent of every asynchronous exception.
data SomeAsyncException = forall e. (Exception e) => SomeAsyncException e

instance Show SomeAsyncException where
  showsPrec _ (SomeAsyncException exception) = showString (displayException exception)

instance Exception SomeAsyncException

asyncExceptionToException :: (Exception e) => e -> SomeException
asyncExceptionToException = toException . SomeAsyncException

asyncExceptionFromException :: (Exception e) => SomeException -> Maybe e
asyncExceptionFromException exception =
  case fromException exception of
    Nothing -> Nothing
    Just wrapped ->
      case wrapped of
        SomeAsyncException inner -> fromException (toException inner)

data AsyncException
  = StackOverflow
  | HeapOverflow
  | ThreadKilled
  | UserInterrupt

asyncExceptionTag :: AsyncException -> Int
asyncExceptionTag StackOverflow = 0
asyncExceptionTag HeapOverflow = 1
asyncExceptionTag ThreadKilled = 2
asyncExceptionTag UserInterrupt = 3

instance Eq AsyncException where
  left == right = asyncExceptionTag left == asyncExceptionTag right

instance Ord AsyncException where
  compare left right = compare (asyncExceptionTag left) (asyncExceptionTag right)

instance Exception AsyncException where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

instance Show AsyncException where
  showsPrec _ StackOverflow = showString "stack overflow"
  showsPrec _ HeapOverflow = showString "heap overflow"
  showsPrec _ ThreadKilled = showString "thread killed"
  showsPrec _ UserInterrupt = showString "user interrupt"

stackOverflow :: SomeException
stackOverflow = toException StackOverflow

heapOverflow :: SomeException
heapOverflow = toException HeapOverflow

data ArrayException
  = IndexOutOfBounds String
  | UndefinedElement String

instance Eq ArrayException where
  IndexOutOfBounds left == IndexOutOfBounds right = left == right
  UndefinedElement left == UndefinedElement right = left == right
  _ == _ = False

instance Exception ArrayException

instance Show ArrayException where
  showsPrec _ (IndexOutOfBounds message) =
    showString "array index out of range" . describe message
  showsPrec _ (UndefinedElement message) =
    showString "undefined array element" . describe message

describe :: String -> String -> String
describe [] = showString ""
describe message = showString ": " . showString message
