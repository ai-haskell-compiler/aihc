{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.TopHandler
  ( flushStdHandles,
    runIO,
    runIOFastExit,
    runMainIO,
    runNonIO,
    topHandler,
    topHandlerFastExit,
    reportError,
    reportStackOverflow,
  )
where

import Control.Exception (SomeException (..), catch, displayException, fromException, someExceptionContext)
import Control.Exception.Context (displayExceptionContext)
import Data.Maybe (Maybe (..))
import Data.Typeable (tyConModule, tyConName, tyConPackage, typeOf, typeRepTyCon)
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (Int#, RealWorld, State#)
import System.Environment (getProgName)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Prelude

-- | Run the main action. The standard handles are flushed when the
-- action returns. The top handler flushes them when the action raises an
-- exception.
runMainIO :: IO a -> IO a
runMainIO action = do
  result <- catch action topHandler
  flushStdHandles
  return result

runIO :: IO a -> IO a
runIO action = catch action topHandler

runIOFastExit :: IO a -> IO a
runIOFastExit action = catch action topHandlerFastExit

runNonIO :: a -> IO a
runNonIO value = catch (value `seq` return value) topHandler

topHandler :: SomeException -> IO a
topHandler exception = catch (handleException safeExit exception) topHandler

topHandlerFastExit :: SomeException -> IO a
topHandlerFastExit exception = catch (handleException fastExit exception) topHandlerFastExit

handleException :: (Int -> IO a) -> SomeException -> IO a
handleException exit exception = do
  flushStdHandles
  case (fromException exception :: Maybe ExitCode) of
    Just ExitSuccess -> exit 0
    Just (ExitFailure status) -> exit status
    Nothing -> do
      reportError exception
      exit 1

reportStackOverflow :: IO ()
reportStackOverflow = writeStderrLine "stack overflow"

-- | Write the exception as the GHC 9.12 top handler does: the program name,
-- the type of the exception, the message, and then the context. The context
-- holds, for example, the call stack of an 'error' call.
reportError :: SomeException -> IO ()
reportError exception = do
  programName <- getProgName
  writeStderrLine
    ( programName
        ++ ": Uncaught exception "
        ++ exceptionTypeName exception
        ++ ":\n\n"
        ++ displayException exception
        ++ case displayExceptionContext (someExceptionContext exception) of
          [] -> []
          context -> "\n\n" ++ context ++ "\n"
    )

-- | The package, the module, and the name of the type of the exception. As
-- GHC does for its own packages, the package has no version.
exceptionTypeName :: SomeException -> String
exceptionTypeName (SomeException inner) =
  let tyCon = typeRepTyCon (typeOf inner)
   in packageWithoutVersion (tyConPackage tyCon) ++ ":" ++ tyConModule tyCon ++ "." ++ tyConName tyCon

-- | Remove a final @-1.2.3@ version from a package name.
packageWithoutVersion :: String -> String
packageWithoutVersion package =
  case break (== '-') (reverse package) of
    (version@(_ : _), '-' : name@(_ : _))
      | all isVersionCharacter version -> reverse name
    _ -> package
  where
    isVersionCharacter character = character == '.' || (character >= '0' && character <= '9')

-- | Flush the standard output handles. An error during the flush is
-- dropped, because the program is about to exit.
flushStdHandles :: IO ()
flushStdHandles = do
  catch (hFlush stdout) ignoreException
  catch (hFlush stderr) ignoreException

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

safeExit :: Int -> IO a
safeExit = exitWithStatus

fastExit :: Int -> IO a
fastExit = exitWithStatus

exitWithStatus :: Int -> IO a
exitWithStatus (I# status) = IO (aihcExit# status)

foreign import prim aihcExit# :: Int# -> State# RealWorld -> (# State# RealWorld, a #)

writeStderrLine :: String -> IO ()
writeStderrLine = hPutStrLn stderr
