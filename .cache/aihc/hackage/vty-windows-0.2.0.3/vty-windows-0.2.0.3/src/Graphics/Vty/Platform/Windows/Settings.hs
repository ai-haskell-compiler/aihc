-- | This module provides data type to describe runtime terminal settings,
-- and a function to obtain default values for those settings.
module Graphics.Vty.Platform.Windows.Settings
  ( WindowsSettings(..)
  , defaultSettings
  )
where

import Data.Maybe ( fromMaybe ) 
import System.Environment ( lookupEnv )
import System.IO ( Handle, stdin, stdout )

-- | Runtime library settings for interacting with Windows terminals.
data WindowsSettings = WindowsSettings
  { settingInputFd :: Handle
  -- ^ The input file descriptor to use.
  , settingOutputFd :: Handle
  -- ^ The output file descriptor to use.
  , settingTermName :: String
  -- ^ The terminal name used to look up terminfo capabilities.
  }
  deriving (Show, Eq)

-- | Description of reasonable default settings for a Windows environment
defaultSettings :: IO WindowsSettings
defaultSettings = do
    mb <- lookupEnv termVariable
    let termName = fromMaybe "xterm-256color" mb

    return $ WindowsSettings { settingInputFd  = stdin
                             , settingOutputFd  = stdout
                             , settingTermName  = termName
                             }

-- | The TERM environment variable
termVariable :: String
termVariable = "TERM"