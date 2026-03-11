-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Customize uncaught exception handler.
--
-- When any thread of your Haskell application throws an exception
-- that does not get caught explicitly, the Haskell runtime system will
-- handle it and print based on the 'Show' instance (by default). This
-- behavior can be customized using the 'setUncaughtExceptionHandler'
-- function. However, implementing your own uncaught exception handler
-- sounds like a tricky task because you should keep a bunch of things
-- in mind:
--
-- 1. You should flush stdout.
-- 2. You should print output to stderr rather than stdout.
-- 3. You should take care of the exit code of your application and the
-- 'ExitCode' data type.
--
-- The aim of this module is to handle all these concerns and provide
-- easy-to-use functions that capture common cases when you want to
-- modify behavior of the default uncaught exception handler.
-- Currently we consider only one case: using 'displayException' instead
-- of the 'Show' methods. It's debatable whether 'displayException' is
-- more appropriate to print uncaught exceptions. Even though
-- 'displayException' is not used by default, we believe there are cases
-- when it makes sense to use it for printing.
--
-- We intentionally provide more than one implementation with similar types
-- and semantics. We expect that over time these functions
-- will be better tested and some feedback will be gathered that
-- will let us figure out which approach is better.

module Control.Exception.Uncaught
  ( displayUncaughtException
  , withDisplayExceptionHandler
  , setDisplayExceptionHandler

  -- * Exported just in case you want to do something special
  , DisplayExceptionInShow (..)
  , wrapException
  ) where

import Control.Exception
  (Deadlock, Exception(..), SomeAsyncException, SomeException, handle, throwIO)
import Data.Maybe (isNothing)
import GHC.Conc.Sync (getUncaughtExceptionHandler, setUncaughtExceptionHandler)
import System.Exit (ExitCode)

-- | Customise default uncaught exception handling to use
-- 'displayException' instead of 'show'. This function is supposed to
-- be applied to the body of @main@. Note that it only affects exceptions
-- in the current thread, other threads are not affected.
-- It is adviced to use the @async@ package that propagates exceptions
-- from the child thread to the parent thread.
--
-- It works by catching all exceptions and wrapping them into
-- a wrapper data type whose 'show' method uses 'displayException'.
-- The wrapped exception is re-thrown and the default exception
-- handler will handle it. 'displayException' will be used for printing
-- because that's how 'show' of the wrapper data type is implemented.
-- Some exceptions are not wrapped:
--
-- 1. 'ExitCode' exception because it's treated specially and affects
-- exit code of the application. If we catch 'ExitSuccess', wrap it into
-- another data type and re-throw, the program will end with non-zero code
-- which is not desirable.
-- 2. Asynchronous exceptions. There are not many of them and applying
-- 'displayException' to them usually does not give big benefit.
-- However, some of them may be handled somewhat specially by the runtime
-- system and we don't want to mess up with that.
-- We recognize asynchronous exceptions by casting to 'SomeAsyncException'
-- the same way as the @safe-exceptions@ library.
displayUncaughtException :: IO a -> IO a
displayUncaughtException = handle (throwIO . wrapException)

-- | Customise default uncaught exception handling to use
-- 'displayException' instead of 'show'. This function is supposed to
-- be applied to the body of @main@.
--
-- It works similarly to 'displayUncaughtException', but instead of
-- catching and throwing exceptions it modifies the uncaught exception
-- handler to wrap the exception before processing it.
-- As a consequence, it affects __all__ threads.
-- When the action finishes, the uncaught exception handler
-- is restored (normally it should not matter because the function is
-- supposed to be applied to the whole @main@).
--
-- Note that it may cause race condition if the passed action spawns another
-- thread that throws an uncaught exception when the passed action stops.
-- There is a global variable that stores the uncaught exception handler.
-- Hence it's recommended to use functions from the @async@ package to spawn
-- threads, so that they are stopped before their parent.
--
-- The handler won't be restored in case of exception thrown by the passed action
-- because otherwise it wouldn't work.
-- Specifically, if we restored the handler in case of
-- exception (e. g. using @bracket@), it would be restored before the
-- uncaught exception handler would be called (because the uncaught
-- exception handler is called after everything).
withDisplayExceptionHandler :: IO a -> IO a
withDisplayExceptionHandler action = do
  handler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler (handler . wrapException)
  action <* setUncaughtExceptionHandler handler

-- | A version of 'withDisplayExceptionHandler' that updates the handler forever.
-- The only difference is that it doesn't restore the default handler.
-- This function should give more predictable behavior in case there are
-- multiple threads.
setDisplayExceptionHandler :: IO ()
setDisplayExceptionHandler = do
  handler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler (handler . wrapException)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Helper data type used by 'displayUncaughtException'.
-- It causes @show@ to call @displayException@.
-- When an exception of this type is caught, it will be @show@n and
-- that will call @displayException@ of the wrapped exception.
newtype DisplayExceptionInShow = DisplayExceptionInShow SomeException

instance Show DisplayExceptionInShow where
  show (DisplayExceptionInShow se) = displayException se

instance Exception DisplayExceptionInShow

wrapException :: SomeException -> SomeException
wrapException e
  | isSyncException e
  , Nothing <- fromException @Deadlock e
  , Nothing <- fromException @ExitCode e =
      toException $ DisplayExceptionInShow e
  | otherwise = e
  where
    isSyncException :: SomeException -> Bool
    isSyncException = isNothing . fromException @SomeAsyncException
