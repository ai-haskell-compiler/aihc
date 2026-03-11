-- |
-- Module      :  ELynx.Tools.Environment
-- Description :  Runtime environment
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  2 22:46:02 2021.
module ELynx.Tools.Environment
  ( Environment (..),
    initializeEnvironment,
    closeEnvironment,
  )
where

import Control.Concurrent.MVar
import Control.Monad
import Data.Time
import ELynx.Tools.InputOutput
import ELynx.Tools.Logger
import ELynx.Tools.Options
import System.IO

-- | The environment of an ELynx run.
data Environment a = Environment
  { -- | Global arguments.
    globalArguments :: GlobalArguments,
    -- | Local arguments of command.
    localArguments :: a,
    -- | List will be empty if using 'Quiet'. If an output base name is
    -- available, 'logHandles' will contain two handles: (1) the standard output
    -- and (2) the log file.
    logHandles :: [Handle],
    -- | MVar blocking output.
    outLock :: MVar (),
    -- | Used to calculate the ETA.
    startingTime :: UTCTime
  }
  deriving (Eq)

instance HasLock (Environment a) where
  getLock = outLock

instance HasLogHandles (Environment a) where
  getLogHandles = logHandles

instance HasStartingTime (Environment a) where
  getStartingTime = startingTime

instance HasVerbosity (Environment a) where
  getVerbosity = verbosity . globalArguments

-- | Initialize the environment.
--
-- Open log file, get current time.
initializeEnvironment :: GlobalArguments -> a -> IO (Environment a)
initializeEnvironment g l = do
  t <- getCurrentTime
  mh <- case (outFileBaseName g, verbosity g) of
    (_, Quiet) -> return []
    (Just bn, _) -> do
      let fn = bn ++ ".log"
      h <- openFileWithExecutionMode em fn
      return [stdout, h]
    (Nothing, _) -> return [stdout]
  lock <- newMVar ()
  return $ Environment g l mh lock t
  where
    em = executionMode g

-- | Close file handles.
closeEnvironment :: Environment s -> IO ()
closeEnvironment e = forM_ hs hClose
  where
    hs = filter (/= stdout) $ logHandles e
