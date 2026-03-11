{-# LANGUAGE RecordWildCards #-}

module Test.Main
  ( -- * Utilities for testing your main function
    captureProcessResult
  , ProcessResult(..)
  , withStdin
  , withEnv

  -- ** Re-export from System.Environment
  , withArgs

  -- ** Re-export from System.Exit
  , ExitCode(..)
  ) where


import qualified Control.Exception as E
import           Control.Monad (mapM, mapM_)
import qualified Data.ByteString as B
import           GHC.IO.Handle (hDuplicate, hDuplicateTo)
import           System.Directory (removeFile, getTemporaryDirectory)
import           System.Environment (withArgs, setEnv, unsetEnv, lookupEnv)
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))
import           System.IO
                   ( Handle
                   , SeekMode(AbsoluteSeek)
                   , stdin
                   , stderr
                   , stdout
                   , hClose
                   , hFlush
                   , hSetBinaryMode
                   , hGetBuffering
                   , hSetBuffering
                   , hGetEncoding
                   , hSetEncoding
                   , hSeek
                   , openBinaryTempFile
                   , withBinaryFile
                   , IOMode(ReadMode)
                   )

import           Test.Main.Internal (ProcessResult(..))


-- |
-- Capture stdout, stderr, and exit code of the given IO action.
--
-- >>> let main = putStr "hello"
-- >>> captureProcessResult main
-- ProcessResult {prStdout = "hello", prStderr = "", prExitCode = ExitSuccess, prException = Nothing}
--
-- If the IO action exit with error message, the exit code of result is 'ExitFailure'.
--
-- >>> import System.IO
-- >>> import System.Exit
-- >>> let main = hPutStr stderr "OMG!" >> exitWith (ExitFailure 1)
-- >>> captureProcessResult main
-- ProcessResult {prStdout = "", prStderr = "OMG!", prExitCode = ExitFailure 1, prException = Nothing}
--
-- @since 0.2.0.0
-- Since v0.2.0.0, this function catches @SomeException@, not only @ExitCode@
-- to prevent it from losing output when an exception other than @ExitCode@ is thrown.
-- To get the thrown error, use @prException@.
--
-- Note: 'prStderr' doesn't contain the error message of the thrown exception.
-- See the example below.
--
-- >>> import Control.Exception
-- >>> let main = ioError $ userError "OMG!"
-- >>> captureProcessResult main
-- ProcessResult {prStdout = "", prStderr = "", prExitCode = ExitFailure 1, prException = Just user error (OMG!)}
captureProcessResult :: IO () -> IO ProcessResult
captureProcessResult action = do
  tDir <- getTemporaryDirectory
  withBinaryTmpFile tDir "test-stdout" $ \(_oPath, oHd) ->
    withBinaryTmpFile tDir "test-stderr" $ \(_ePath, eHd) ->
      redirectingHandle stdout oHd $
        redirectingHandle stderr eHd $ do
          (prExitCode, prException) <- captureExitCodeAndException action
          prStdout <- readFromHead oHd stdout
          prStderr <- readFromHead eHd stderr
          return ProcessResult {..}

  where
    readFromHead tmpH stdH = do
      hFlush stdH
      hSeek tmpH AbsoluteSeek 0
      B.hGetContents tmpH

    captureExitCodeAndException act =
      (act >> return (ExitSuccess, Nothing))
        `E.catches` [E.Handler forExitCode, E.Handler forSomeException]

    forExitCode :: ExitCode -> IO (ExitCode, Maybe E.SomeException)
    forExitCode eCode = return (eCode, Nothing)

    forSomeException :: E.SomeException -> IO (ExitCode, Maybe E.SomeException)
    forSomeException ex = return (ExitFailure 1, Just ex)


withBinaryTmpFile :: FilePath -> String -> ((FilePath, Handle) -> IO a) -> IO a
withBinaryTmpFile parent name =
  E.bracket
    (openBinaryTempFile parent name)
    (\(path, hd) -> do
      hClose hd
      removeFile path `E.catch` doNothing
    )

  where
    doNothing :: IOError-> IO ()
    doNothing _ = return ()


redirectingHandle :: Handle -> Handle -> IO r -> IO r
redirectingHandle from to action = do
  saveEnc <- hGetEncoding from
  saveBuf <- hGetBuffering from
  let redirect = do
        save <- hDuplicate from
        hDuplicateTo to from
        setEnc to
        return save
      restore save = do
        hDuplicateTo save from
        hClose save
        setEnc from
        hSetBuffering from saveBuf

      setEnc h =
        maybe (hSetBinaryMode h True) (hSetEncoding h) saveEnc
  E.bracket redirect restore (const action)


-- |
-- Pass the ByteString to stdin of the given IO action.
--
-- >>> import Data.ByteString.Char8 ()
-- >>> :set -XOverloadedStrings
-- >>> let main = putStrLn . reverse =<< getLine
-- >>> withStdin "abcde" main
-- edcba
withStdin :: B.ByteString -> IO a -> IO a
withStdin bs action =
  E.bracket
    prepareInputFile
    removeFile
    (\inPath ->
      withBinaryFile inPath ReadMode $ \tmpHd ->
        redirectingHandle stdin tmpHd action
    )
  where
    prepareInputFile = do
      tDir <- getTemporaryDirectory
      E.bracket
        (openBinaryTempFile tDir "test-stdin")
        (\(_path, hd) -> hClose hd)
        (\(path, hd) -> B.hPut hd bs >> return path)


-- |
-- Run the given IO action with the specified environment variables set.
-- The environment variables are specified as pairs of @ENV_VAR_NAME@ and @ENV_VAR_VALUE@.
-- If @ENV_VAR_VALUE@ is @Nothing@, the @ENV_VAR_NAME@ is unset with 'unsetEnv'.
--
-- >>> import System.Environment
-- >>> setEnv "ENV_VAR_TO_UNSET"    "value_to_unset"
-- >>> setEnv "ENV_VAR_TO_OVERWRITE" "value_to_overwrite"
-- >>> let main = (print =<< lookupEnv "ENV_VAR_TO_UNSET") >> (print =<< lookupEnv "ENV_VAR_TO_OVERWRITE")
-- >>> withEnv [("ENV_VAR_TO_UNSET", Nothing), ("ENV_VAR_TO_OVERWRITE" , Just "new_value")] main
-- Nothing
-- Just "new_value"
-- >>> main
-- Just "value_to_unset"
-- Just "value_to_overwrite"
withEnv :: [(String, Maybe String)] -> IO a -> IO a
withEnv newVarVals action =
  E.bracket
    (saveReplaces newVarVals)
    replaces
    (const action)

  where
    saveReplaces = mapM $ \varVal@(var, _val) ->
      ((,) <$> pure var <*> lookupEnv var) <* replace varVal

    replaces = mapM_ replace

    replace (var, Just val) = setEnv var val
    replace (var, Nothing)  = unsetEnv var
