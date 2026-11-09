-- | Environment access that allows empty values, with the signatures of
-- @System.Posix.Env@.
module System.Environment.Blank
  ( getArgs,
    getEnv,
    getEnvDefault,
    getEnvironment,
    getExecutablePath,
    getProgName,
    setEnv,
    unsetEnv,
    withArgs,
    withProgName,
  )
where

import Data.Maybe (fromMaybe)
import GHC.IO.Exception (IOErrorType (..), ioError)
import GHC.Internal.Environment (getFullEnvironment, setFullEnvironment)
import System.Environment
  ( getArgs,
    getEnvironment,
    getExecutablePath,
    getProgName,
    lookupEnv,
    withArgs,
    withProgName,
  )
import System.IO.Error (mkIOError)
import Prelude

-- | The value of an environment variable, or 'Nothing' when the process
-- environment does not name it.
getEnv :: String -> IO (Maybe String)
getEnv = lookupEnv

-- | The value of an environment variable, or the default value when the
-- process environment does not name it.
getEnvDefault :: String -> String -> IO String
getEnvDefault name fallback = fromMaybe fallback <$> lookupEnv name

-- | Give a value to an environment variable. An empty value is a valid
-- value. When the flag is 'False', a variable that exists keeps its value.
-- Each part stops at its first NUL character. The action fails when the
-- name is empty or contains an equals sign.
setEnv :: String -> String -> Bool -> IO ()
setEnv name value overwrite
  | invalidName key = ioError (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | overwrite = replace
  | otherwise = do
      existing <- lookupEnv key
      case existing of
        Nothing -> replace
        Just _ -> return ()
  where
    key = takeWhile (/= '\NUL') name
    entryValue = takeWhile (/= '\NUL') value
    replace = do
      entries <- getFullEnvironment
      setFullEnvironment (removeName key entries ++ [key ++ "=" ++ entryValue])

-- | Remove an environment variable. The action fails when the name is empty
-- or contains an equals sign.
unsetEnv :: String -> IO ()
unsetEnv name
  | invalidName name = ioError (mkIOError InvalidArgument "unsetEnv" Nothing Nothing)
  | otherwise = do
      entries <- getFullEnvironment
      setFullEnvironment (removeName name entries)

invalidName :: String -> Bool
invalidName name = null name || '=' `elem` name

-- | Remove each @NAME=VALUE@ entry with the given name. An entry without an
-- equals sign is a name with an empty value.
removeName :: String -> [String] -> [String]
removeName name = filter ((/= name) . takeWhile (/= '='))
