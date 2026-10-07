-- |
-- Module      : Aihc.Cli.Terminal
-- Description : The terminal facts of a build with the Cabal flag +pretty-ui
--
-- The Cabal flag @pretty-ui@ selects this module or the one under
-- @pretty-ui/disabled@. Both have the same interface.
module Aihc.Cli.Terminal
  ( liveTerminal,
    terminalSize,
  )
where

import System.Console.Terminal.Size qualified as Terminal
import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice)

-- | Whether the handle is a terminal that can show output that redraws in
-- place. A terminal with @TERM@ set to @dumb@ cannot.
liveTerminal :: Handle -> IO Bool
liveTerminal handle = do
  isTerminal <- hIsTerminalDevice handle
  term <- lookupEnv "TERM"
  pure (isTerminal && term /= Just "dumb")

-- | The columns and rows of the terminal. A terminal that reports no size,
-- such as a pseudo-terminal without a window, counts as 80 by 24.
terminalSize :: Handle -> IO (Int, Int)
terminalSize handle = do
  window <- Terminal.hSize handle
  let known fallback value = if value > 0 then value else fallback
  pure (maybe 80 (known 80 . Terminal.width) window, maybe 24 (known 24 . Terminal.height) window)
