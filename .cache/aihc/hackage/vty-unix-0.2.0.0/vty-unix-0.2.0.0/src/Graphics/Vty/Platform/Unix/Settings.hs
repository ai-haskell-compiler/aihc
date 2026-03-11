{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Runtime settings for @vty-unix@. Most applications will not need to
-- change any of these settings.
module Graphics.Vty.Platform.Unix.Settings
  ( VtyUnixConfigurationError(..)
  , UnixSettings(..)
  , currentTerminalName
  , defaultSettings
  )
where

import Control.Exception (Exception(..), throwIO)
import Control.Monad (when, void)
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Typeable (Typeable)
import System.Environment (lookupEnv)
import System.IO (Handle, BufferMode(..), hReady, hSetBuffering, hGetChar, stdin)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Types (Fd(..))

-- | Type of exceptions that can be raised when configuring Vty on a
-- Unix system.
data VtyUnixConfigurationError =
    MissingTermEnvVar
    -- ^ The @TERM@ environment variable is not set.
    deriving (Show, Eq, Typeable)

instance Exception VtyUnixConfigurationError where
    displayException MissingTermEnvVar = "TERM environment variable not set"

-- | Runtime library settings for interacting with Unix terminals.
--
-- See this page for details on @VTIME@ and @VMIN@:
--
-- http://unixwiz.net/techtips/termios-vmin-vtime.html
data UnixSettings =
    UnixSettings { settingVmin :: Int
                 -- ^ VMIN character count.
                 , settingVtime :: Int
                 -- ^ VTIME setting in tenths of a second.
                 , settingInputFd :: Fd
                 -- ^ The input file descriptor to use.
                 , settingOutputFd :: Fd
                 -- ^ The output file descriptor to use.
                 , settingTermName :: String
                 -- ^ The terminal name used to look up terminfo capabilities.
                 }
                 deriving (Show, Eq)

-- | Default runtime settings used by the library.
defaultSettings :: IO UnixSettings
defaultSettings = do
    mb <- lookupEnv termVariable
    case mb of
      Nothing -> throwIO MissingTermEnvVar
      Just t -> do
        flushStdin
        return $ UnixSettings { settingVmin      = 1
                              , settingVtime     = 100
                              , settingInputFd   = stdInput
                              , settingOutputFd  = stdOutput
                              , settingTermName  = t
                              }

termVariable :: String
termVariable = "TERM"

currentTerminalName :: IO (Maybe String)
currentTerminalName = lookupEnv termVariable

flushStdin :: IO ()
flushStdin = do
    hSetBuffering stdin NoBuffering
    whileM $ consume stdin

whileM :: (Monad m) => m Bool -> m ()
whileM act = do
    continue <- act
    when continue $ whileM act

consume :: Handle -> IO Bool
consume h = do
    avail <- hReady h
    when avail $ void $ hGetChar h
    return avail
