{-# LANGUAGE RecordWildCards, CPP #-}
-- | This module provides a function to build an 'Output' for Unix
-- terminal devices.
--
-- This module is exposed for testing purposes only; applications should
-- never need to import this directly.
module Graphics.Vty.Platform.Unix.Output
  ( buildOutput
  )
where

import Graphics.Vty.Config
import Graphics.Vty.Output

import Graphics.Vty.Platform.Unix.Settings
import Graphics.Vty.Platform.Unix.Output.Color (detectColorMode)
import Graphics.Vty.Platform.Unix.Output.XTermColor as XTermColor
import Graphics.Vty.Platform.Unix.Output.TerminfoBased as TerminfoBased

import Data.List (isPrefixOf)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- | Returns an 'Output' for the terminal specified in 'UnixSettings'.
--
-- The specific output implementation chosen is based
-- on the @TERM@ environment variable and ultimately
-- uses @Graphics.Vty.Platform.Unix.Output.XTermColor@
-- for terminals that look @xterm@-like or
-- @Graphics.Vty.Platform.Unix.Output.TerminfoBased@ as a fallback
-- otherwise.
--
-- * If @TERM@ starts with @xterm@, @screen@, @rxvt@, or @tmux@, this
--   will the @xterm@-based implementation.
-- * Otherwise this will use the 'TerminfoBased' implementation.
buildOutput :: VtyUserConfig -> UnixSettings -> IO Output
buildOutput config settings = do
    let termName = settingTermName settings
        fd = settingOutputFd settings

    colorMode <- case configPreferredColorMode config of
        Nothing -> detectColorMode termName
        Just m -> return m

    t <- if isXtermLike termName
         then XTermColor.reserveTerminal termName fd colorMode
         -- Not an xterm-like terminal. try for generic terminfo.
         else TerminfoBased.reserveTerminal termName fd colorMode

    return t

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
