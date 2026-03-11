{-# LANGUAGE DeriveGeneric #-}

module Data.Monoid.Colorful.Term (
  Term(..)
  , getTerm
  , hGetTerm
) where

import Data.List (isPrefixOf, isInfixOf)
import System.Environment (getEnv)
import System.IO (Handle, hIsTerminalDevice, stdout)
import GHC.Generics (Generic)

-- | Terminal type. For less capable terminals the color depth is automatically reduced.
data Term
  = TermDumb -- ^ Dumb terminal - no color output
  | Term8    -- ^ 8 colors supported
  | Term256  -- ^ 256 colors supported
  | TermRGB  -- ^ True colors supported
  | TermWin  -- ^ Windows terminal. Will use emulation (Not yet implemented).
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

getTerm :: IO Term
getTerm = hGetTerm stdout

-- | The action @(hGetTerm handle)@ determines the terminal type of the file @handle@.
--
-- The terminal type is determined by checking if the file handle points to a device
-- and by looking at the @$TERM@ environment variable.
hGetTerm :: Handle -> IO Term
hGetTerm h = do
  term <- hIsTerminalDevice h
  if term
    then envToTerm  <$> getEnv "TERM"
    else pure TermDumb

-- | Determine the terminal type from the value of the @$TERM@ environment variable.
-- TODO improve this
envToTerm :: String -> Term
envToTerm "dumb" = TermDumb
envToTerm term | any (`isPrefixOf` term) rgbTerminals = TermRGB
               | "256" `isInfixOf` term = Term256
               | otherwise = Term8
  where rgbTerminals = ["xterm", "konsole", "gnome", "st", "linux"]
