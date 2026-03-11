{-# LANGUAGE CPP #-}

module System.Environment.XDG.UserDir
    (readUserDirs,
     getUserDir) where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import System.FilePath
import System.Environment
import System.Environment.XDG.BaseDir
import System.Directory

-- | Element of shell-string
data Element = Fixed Char | Var String
  deriving (Eq, Show)

-- | Parse shell-format string
parseString :: String -> [Element]
parseString [] = []
parseString ('$':xs) =
  case break (`elem` "!#/;:,.*?%-=$<> \r\n\t") xs of
    ([], cs) -> Fixed '$': parseString cs
    (name, []) -> [Var name]
    (name, cs) -> Var name: parseString cs
parseString (x:xs) = Fixed x: parseString xs

-- | Render shell-format string using given environment
renderElements :: [(String, String)] -> [Element] -> String
renderElements env list = concatMap render list
  where
    render (Fixed c) = [c]
    render (Var name) =
      fromMaybe "" $ lookup name env 

-- | Split list
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split sep list =
  case span (/= sep) list of
    (x, []) -> [x]
    (x, s:xs)
      | s == sep  -> x: split sep xs
      | otherwise -> [x, s:xs]

-- | Similar to System.Environment.getEnv,
-- but returns empty string if there is no
-- such variable.
getEnv' :: String -> IO String
getEnv' var = do
  env <- getEnvironment
  return $ fromMaybe "" $ lookup var env

-- | Check if line is not a comment
notComment :: String -> Bool
notComment [] = False
notComment ('#':_) = False
notComment _ = True

-- | Parse `NAME=VALUE' pair
parsePair :: String -> Maybe (String, String)
parsePair str =
  case span (/= '=') str of
    (name, '=':value) -> Just (name, stripQuotes value)
    _ -> Nothing

-- | Strip single\/double quotes
stripQuotes :: String -> String
stripQuotes [] = []
stripQuotes s@('"':xs) =
  if last xs == '"' then init xs else s
stripQuotes s@('\'':xs) =
  if last xs == '\'' then init xs else s
stripQuotes s = s

-- | Read list of `NAME=VALUE' pairs from file.
-- If there is no such file, return empty list.
readPairs :: FilePath -> IO [(String,String)]
readPairs path = do
  b <- doesFileExist path
  if b
    then do
         str <- readFile path
         let ls = filter notComment (lines str)
         return $ mapMaybe parsePair ls
    else return []

-- | Read default XDG-user-dirs config
readDefaults :: IO (M.Map String String)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
readDefaults = return M.empty
#else
readDefaults = do
  pairs <- readPairs "/etc/xdg/user-dirs.defaults"
  return $ M.fromList pairs
#endif

xdgVar :: [(String, String)] -> (String, String) -> Maybe (String, String)
xdgVar env (name, value) =
  case split '_' name of
    ["XDG", var, "DIR"] -> Just (var, renderElements env $ parseString value)
    _ -> Nothing

-- | Read user-configured set of user directories
-- (from user-dirs.dirs)
readUserDirs :: IO (M.Map String String)
readUserDirs = do
  configDir <- getUserConfigDir ""
  let userConfig = configDir </> "user-dirs.dirs"
  pairs <- readPairs userConfig
  env <- getEnvironment
  return $ M.fromList $ mapMaybe (xdgVar env) pairs

-- | Get one specific user directory (e. g., 
-- getUserDir \"DOWNLOAD\"). If there is no
-- such specified directory, return home directory.
getUserDir :: String -> IO String
getUserDir name = do
  home <- getHomeDirectory
  def <- readDefaults
  user <- readUserDirs
  case M.lookup name (user `M.union` def) of
    Nothing -> return home
    Just val -> case val of
                  ('/':_) -> return val
                  _ -> return (home </> val)

