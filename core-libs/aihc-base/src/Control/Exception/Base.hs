{-# LANGUAGE PatternSynonyms #-}

-- The export list bundles the ErrorCall pattern with its type. The GHC
-- parser needs PatternSynonyms for that export form, but HLint does not
-- see it as a use.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Control.Exception.Base
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    ErrorCall (.., ErrorCall),
    IOException,
    IOError,
    ioError,
    ArrayException (..),
    AssertionFailed (..),
    AsyncException (..),
    SomeAsyncException (..),
    asyncExceptionToException,
    asyncExceptionFromException,
    BlockedIndefinitelyOnMVar (..),
    BlockedIndefinitelyOnSTM (..),
    Deadlock (..),
    AllocationLimitExceeded (..),
    NestedAtomically (..),
    NoMethodError (..),
    NonTermination (..),
    PatternMatchFail (..),
    RecConError (..),
    RecSelError (..),
    RecUpdError (..),
    TypeError (..),
    CompactionFailed (..),
    catch,
    catchJust,
    evaluate,
    handle,
    handleJust,
    throw,
    throwIO,
    throwTo,
    try,
    tryJust,
    mapException,
    onException,
    finally,
    bracket,
    bracket_,
    bracketOnError,
    assert,
    mask,
    mask_,
    uninterruptibleMask,
    uninterruptibleMask_,
    MaskingState (..),
    getMaskingState,
    interruptible,
  )
where

import GHC.Base (assert)
import GHC.Conc.Sync (throwTo)
import GHC.Exception (ArithException (..), ErrorCall (..), Exception (..), SomeException (..), throw)
import GHC.IO (MaskingState (..), bracket, catch, evaluate, finally, getMaskingState, interruptible, mask, mask_, onException, throwIO, uninterruptibleMask, uninterruptibleMask_)
import GHC.IO.Exception
  ( AllocationLimitExceeded (..),
    ArrayException (..),
    AssertionFailed (..),
    AsyncException (..),
    BlockedIndefinitelyOnMVar (..),
    BlockedIndefinitelyOnSTM (..),
    CompactionFailed (..),
    Deadlock (..),
    IOError,
    IOException,
    SomeAsyncException (..),
    asyncExceptionFromException,
    asyncExceptionToException,
    ioError,
  )
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Show (Show (..), showString)
import Prelude (Either (..), IO, Maybe (..), String, const, pure, (.), (>>=))

handle :: (Exception e) => (e -> IO a) -> IO a -> IO a
handle handler action = catch action handler

catchJust :: (Exception e) => (e -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust predicate action handler =
  catch action (selectJust predicate handler)

selectJust :: (Exception e) => (e -> Maybe b) -> (b -> IO a) -> e -> IO a
selectJust predicate handler exception =
  case predicate exception of
    Just value -> handler value
    Nothing -> throwIO exception

handleJust :: (Exception e) => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust predicate handler action = catchJust predicate action handler

try :: (Exception e) => IO a -> IO (Either e a)
try action =
  catch
    (action >>= \value -> pure (Right value))
    (pure . Left)

tryJust :: (Exception e) => (e -> Maybe b) -> IO a -> IO (Either b a)
tryJust predicate action =
  catchJust
    predicate
    (action >>= \value -> pure (Right value))
    (pure . Left)

-- | Map one exception that a lazy value throws to another exception.
mapException :: (Exception e1, Exception e2) => (e1 -> e2) -> a -> a
mapException f value = unsafePerformIO (catch (evaluate value) (throwIO . f))

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after use = bracket before (const after) (const use)

bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError acquire release use = do
  resource <- acquire
  use resource `onException` release resource

-- | A pattern match failed. The text gives the source location.
newtype PatternMatchFail = PatternMatchFail String

instance Show PatternMatchFail where
  showsPrec _ (PatternMatchFail message) = showString message

instance Exception PatternMatchFail

-- | A record selector got a constructor that does not have the field.
newtype RecSelError = RecSelError String

instance Show RecSelError where
  showsPrec _ (RecSelError message) = showString message

instance Exception RecSelError

-- | A record construction did not give a value to a strict field.
newtype RecConError = RecConError String

instance Show RecConError where
  showsPrec _ (RecConError message) = showString message

instance Exception RecConError

-- | A record update got a constructor that does not have the field.
newtype RecUpdError = RecUpdError String

instance Show RecUpdError where
  showsPrec _ (RecUpdError message) = showString message

instance Exception RecUpdError

-- | A class method has no definition and no default.
newtype NoMethodError = NoMethodError String

instance Show NoMethodError where
  showsPrec _ (NoMethodError message) = showString message

instance Exception NoMethodError

-- | A deferred type error.
newtype TypeError = TypeError String

instance Show TypeError where
  showsPrec _ (TypeError message) = showString message

instance Exception TypeError

-- | The program cannot terminate.
data NonTermination = NonTermination

instance Show NonTermination where
  showsPrec _ NonTermination = showString "<<loop>>"

instance Exception NonTermination

-- | A call of @atomically@ is inside a different call of @atomically@.
data NestedAtomically = NestedAtomically

instance Show NestedAtomically where
  showsPrec _ NestedAtomically = showString "Control.Concurrent.STM.atomically was nested"

instance Exception NestedAtomically
