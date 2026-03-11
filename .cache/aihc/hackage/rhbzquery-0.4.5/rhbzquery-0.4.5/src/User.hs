{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: GPL-2.0-or-later

module User (
  getRhBzUser
  )
where

import Control.Monad.Extra
import qualified Data.ByteString.Char8 as B
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Ini.Config
import qualified Data.Text as T
import qualified Data.Text.IO as T
import SimplePrompt (promptNonEmpty)
import System.Directory
import System.Environment
import System.FilePath
import qualified Text.Email.Validate as Email

import Bugzilla

eitherRhBzUser :: IO (Either FilePath String)
eitherRhBzUser = do
  home <- getEnv "HOME"
  let rc = home </> ".bugzillarc"
  -- FIXME assumption if file exists then it has b.r.c user
  ifM (doesFileExist rc)
    (Right <$> readIniConfig rc rcParser id)
    (return $ Left rc)
  where
    rcParser :: IniParser String
    rcParser =
      section (T.pack brc) $
      fieldOf (T.pack "user") string

    readIniConfig :: FilePath -> IniParser a -> (a -> b) -> IO b
    readIniConfig inifile iniparser fn = do
      ini <- T.readFile inifile
      return $ either error fn $ parseIniFile ini iniparser

getRhBzUser :: IO String
getRhBzUser = do
  euser <- eitherRhBzUser
  case euser of
    Right user -> return user
    Left rc -> do
      email <- promptNonEmpty "Bugzilla Username"
      when (emailIsValid email) $ do
        writeFile rc $ "[" <> brc <> "]\nuser = " <> email <> "\n"
        putStrLn $ "Saved in " ++ rc
      getRhBzUser
  where
    emailIsValid :: String -> Bool
    emailIsValid = Email.isValid . B.pack
