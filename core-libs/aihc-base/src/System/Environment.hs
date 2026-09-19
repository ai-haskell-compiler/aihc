-- | Process argument and environment access. The scoped mutation operations
-- update the same process-global vector observed by every Haskell thread.
module System.Environment
  ( getArgs,
    getEnv,
    getEnvironment,
    getProgName,
    lookupEnv,
    withArgs,
    withProgName,
  )
where

import Control.Exception.Base (SomeException, catch, throwIO)
import GHC.IO.Exception (IOErrorType (..), ioError)
import GHC.Internal.Environment (getFullArgs, getFullEnvironment, setFullArgs)
import System.IO.Error (mkIOError)
import Prelude

-- | The value of an environment variable, or 'Nothing' when the process
-- environment does not name it.
lookupEnv :: String -> IO (Maybe String)
lookupEnv name = lookupName <$> getEnvironment
  where
    lookupName [] = Nothing
    lookupName ((entryName, value) : rest)
      | entryName == name = Just value
      | otherwise = lookupName rest

-- | The value of an environment variable. The action fails when the process
-- environment does not name it.
getEnv :: String -> IO String
getEnv name = do
  value <- lookupEnv name
  case value of
    Just found -> return found
    Nothing -> ioError (mkIOError NoSuchThing "getEnv" Nothing (Just name))

-- | Every environment variable of the process, paired with its value.
getEnvironment :: IO [(String, String)]
getEnvironment = map splitEntry <$> getFullEnvironment

-- | Split a @NAME=VALUE@ entry. An entry without an equals sign is a name
-- with an empty value, which is how @System.Posix.Env@ reads one too.
splitEntry :: String -> (String, String)
splitEntry = go []
  where
    go name [] = (reverseString name, [])
    go name ('=' : value) = (reverseString name, value)
    go name (character : rest) = go (character : name) rest

-- | Return every initial or replacement argument after the program name.
getArgs :: IO [String]
getArgs = do
  arguments <- getFullArgs
  case arguments of
    [] -> return []
    _programName : rest -> return rest

-- | Return the final path component of the current program name.
getProgName :: IO String
getProgName = do
  arguments <- getFullArgs
  case arguments of
    [] -> return []
    programName : _ -> return (baseName programName)

-- | Run an action with replacement arguments, restoring the previous vector
-- after normal completion or a synchronous exception.
withArgs :: [String] -> IO a -> IO a
withArgs arguments action = do
  oldArguments <- getFullArgs
  let programName =
        case oldArguments of
          [] -> []
          name : _ -> name
  withFullArgs (programName : arguments) oldArguments action

-- | Run an action with a replacement program name, restoring it afterwards.
withProgName :: String -> IO a -> IO a
withProgName programName action = do
  oldArguments <- getFullArgs
  let arguments =
        case oldArguments of
          [] -> []
          _oldProgramName : rest -> rest
  withFullArgs (programName : arguments) oldArguments action

withFullArgs :: [String] -> [String] -> IO a -> IO a
withFullArgs newArguments oldArguments action = do
  setFullArgs newArguments
  result <- restoreOnException action (setFullArgs oldArguments)
  setFullArgs oldArguments
  return result

restoreOnException :: IO a -> IO () -> IO a
restoreOnException action cleanup = catch action handler
  where
    handler :: SomeException -> IO a
    handler exception = do
      cleanup
      throwIO exception

baseName :: String -> String
baseName = go []
  where
    go latest [] = reverseString latest
    go _ ('/' : rest) = go [] rest
    go latest (character : rest) = go (character : latest) rest

reverseString :: String -> String
reverseString = go []
  where
    go :: String -> String -> String
    go reversed [] = reversed
    go reversed (character : rest) = go (character : reversed) rest
