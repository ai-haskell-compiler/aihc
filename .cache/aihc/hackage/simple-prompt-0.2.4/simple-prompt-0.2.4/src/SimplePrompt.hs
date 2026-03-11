{-# LANGUAGE CPP #-}

module SimplePrompt (
  prompt,
  promptInitial,
  promptBuffered,
  promptNonEmpty,
  promptChar,
  promptKeyPress,
  promptEnter,
  promptPassword,
  yesNo,
  yesNoDefault
  ) where

import Control.Monad (void)
import Data.List.Extra (lower, trimEnd)
import Safe
import System.Console.Haskeline (waitForAnyKey)

import SimplePrompt.Internal

#include "monadconstraint.h"

-- FIXME use haveTerminalUI ?
-- | prompt which drops buffered input (using `clearedInput`)
--
-- Ignores buffered input lines (ie if input line gotten in under 5ms)
prompt :: MONADCONSTRAINT => String -> m String
prompt = runPrompt . clearedInput . getPromptLine

-- FIXME non-empty?
-- | reads string with initial input (using `clearedInput`)
promptInitial :: MONADCONSTRAINT => String -> String -> m String
promptInitial s = runPrompt . clearedInput . getPromptInitial s

-- | reads string including any buffered input
promptBuffered :: MONADCONSTRAINT => String -> m String
promptBuffered = runPrompt . getPromptLine

-- | reads non-empty string (using `nonEmptyInput`)
promptNonEmpty :: MONADCONSTRAINT => String -> m String
promptNonEmpty = runPrompt . nonEmptyInput . getPromptLine

-- | prompt for a password
promptPassword :: MONADCONSTRAINT => String -> m String
promptPassword = runPrompt . nonEmptyInput . getPromptPassword

-- | prompt for a printable character
promptChar :: MONADCONSTRAINT => String -> m Char
promptChar =
  runPrompt . clearedInput . getPromptChar

-- | prompt for key press (returns False if Ctrl-d or EOF)
promptKeyPress :: MONADCONSTRAINT => String -> m Bool
promptKeyPress =
  runPrompt . clearedInput . waitForAnyKey

-- | prompt for Enter key
promptEnter :: MONADCONSTRAINT => String -> m ()
promptEnter =
  void . runPrompt . untilInput (== "") . clearedInput . getPromptLine

-- | Yes-No prompt (accepts only {y,n,yes,no} case-insensitive)
yesNo :: MONADCONSTRAINT => String -> m Bool
yesNo desc =
  runPrompt . mapInput maybeYN . getPromptLine $ appendQuestion desc ++ " [y/n]"
  where
    maybeYN inp =
      case lower inp of
        "y" -> Just True
        "yes" -> Just True
        "n" -> Just False
        "no" -> Just False
        _ ->  Nothing

appendQuestion :: String -> String
appendQuestion desc =
  case lastMay (trimEnd desc) of
    Just '?' -> desc
    _ -> desc ++ "?"

-- | Yes-No prompt with default (uses `clearedInput`)
yesNoDefault :: MONADCONSTRAINT => Bool -- ^ True => [Y/n], False => [y,N]
             -> String -> m Bool
yesNoDefault yes desc =
  runPrompt . mapInput maybeYN' . clearedInput . getPromptLine $
  appendQuestion desc ++ " " ++ if yes then "[Y/n]" else "[y/N]"
  where
    maybeYN' inp =
      case lower inp of
        "" -> Just yes
        "y" -> Just True
        "yes" -> Just True
        "n" -> Just False
        "no" -> Just False
        _ ->  Nothing
