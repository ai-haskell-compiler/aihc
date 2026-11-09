module EnvironmentBlankChecks (environmentBlankChecks) where

import System.Environment.Blank (getEnv, getEnvDefault, setEnv, unsetEnv)

-- | An empty value is a value, and 'setEnv' with 'False' keeps an existing
-- value.
environmentBlankChecks :: IO Bool
environmentBlankChecks = do
  setEnv "AIHC_BLANK_CHECK" "" True
  blank <- getEnv "AIHC_BLANK_CHECK"
  setEnv "AIHC_BLANK_CHECK" "ignored" False
  kept <- getEnv "AIHC_BLANK_CHECK"
  setEnv "AIHC_BLANK_CHECK" "new" True
  replaced <- getEnv "AIHC_BLANK_CHECK"
  unsetEnv "AIHC_BLANK_CHECK"
  removed <- getEnv "AIHC_BLANK_CHECK"
  fallback <- getEnvDefault "AIHC_BLANK_CHECK" "fallback"
  pure
    ( blank == Just ""
        && kept == Just ""
        && replaced == Just "new"
        && removed == Nothing
        && fallback == "fallback"
    )
