{-# LANGUAGE RecordWildCards, CPP #-}
-- | This module provides a function to build an 'Output' for Windows
-- terminals.
--
-- This module is exposed for testing purposes only; applications should
-- never need to import this directly.
module Graphics.Vty.Platform.Windows.Output
  ( buildOutput
  )
where

import Graphics.Vty.Config
import Graphics.Vty.Platform.Windows.Settings
import Graphics.Vty.Platform.Windows.Output.Color (detectColorMode)
import Graphics.Vty.Platform.Windows.Output.XTermColor as XTermColor
import Graphics.Vty.Platform.Windows.Output.TerminfoBased as TerminfoBased
import Graphics.Vty.Output

import Data.List (isPrefixOf)

-- | Returns an `Output` for the terminal specified in `WindowsSettings`.
--
-- The specific Output implementation used is hidden from the API user.
-- All terminal implementations are assumed to perform more, or less,
-- the same. Currently, all implementations use terminfo for at least
-- some terminal specific information.
--
-- If a terminal implementation is developed for a terminal without
-- terminfo support then Vty should work as expected on that terminal.
--
-- Selection of a terminal is done as follows:
--
--      * If TERM starts with "xterm", "screen" or "tmux", use XTermColor.
--      * otherwise use the TerminfoBased driver.
buildOutput :: VtyUserConfig -> WindowsSettings -> IO Output
buildOutput config settings = do
    let outHandle = settingOutputFd settings
        termName = settingTermName settings

    colorMode <- case configPreferredColorMode config of
        Nothing -> detectColorMode termName
        Just m -> return m

    if isXtermLike termName
        then XTermColor.reserveTerminal termName outHandle colorMode
        -- Not an xterm-like terminal. try for generic terminfo.
        else TerminfoBased.reserveTerminal termName outHandle colorMode


isXtermLike :: String -> Bool
isXtermLike termName =
    any (`isPrefixOf` termName) xtermLikeTerminalNamePrefixes

xtermLikeTerminalNamePrefixes :: [String]
xtermLikeTerminalNamePrefixes =
    [ "xterm"
    , "screen"
    , "tmux"
    , "rxvt"
    ]
