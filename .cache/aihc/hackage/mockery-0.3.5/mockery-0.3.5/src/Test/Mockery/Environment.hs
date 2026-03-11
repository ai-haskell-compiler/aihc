module Test.Mockery.Environment (
  withEnvironment
, withModifiedEnvironment
) where

import           Control.Exception.Base
import           Control.Monad
import           System.Environment.Compat

-- |
-- Run the given action within the specified environment.
--
-- Before executing the action, `withEnvironment` backs up the current
-- environment, clears out the environment, and then applies the supplied
-- environment.
-- After the action has completed the original environment is restored.
--
-- __Note__: The environment is global to a process, so tests that modify the
-- environment can no longer be run in parallel.
withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment environment action = bracketEnvironment $ do
  setEnvironment environment
  action

-- |
-- Run the given action within an augmented environment.
--
-- Before executing the action, `withModifiedEnvironment` backs up the current
-- environment and then augments it with the supplied values.
-- After the action has completed the original environment is restored.
--
-- __Note__: The environment is global to a process, so tests that modify the
-- environment can no longer be run in parallel.
withModifiedEnvironment :: [(String, String)] -> IO a -> IO a
withModifiedEnvironment environment action = bracketEnvironment $ do
  extendEnvironment environment
  action

bracketEnvironment :: IO a -> IO a
bracketEnvironment = bracket getEnvironment setEnvironment . const

setEnvironment :: [(String, String)] -> IO ()
setEnvironment environment = do
  clearEnvironment
  extendEnvironment environment

extendEnvironment :: [(String, String)] -> IO ()
extendEnvironment environment = forM_ environment $ uncurry setEnv

clearEnvironment :: IO ()
clearEnvironment = do
  environment <- getEnvironment
  forM_ environment (unsetEnv . fst)
