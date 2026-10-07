-- |
-- Module      : Aihc.Cli.Terminal
-- Description : The terminal facts of a build with the Cabal flag -pretty-ui
--
-- The Cabal flag @pretty-ui@ selects this module or the one under
-- @pretty-ui/enabled@. Both have the same interface. This build does not
-- depend on @terminal-size@, and it makes no assumption that a handle is a
-- terminal. The progress output is always one line for each event.
module Aihc.Cli.Terminal
  ( liveTerminal,
    terminalSize,
  )
where

import System.IO (Handle)

-- | No handle shows output that redraws in place.
liveTerminal :: Handle -> IO Bool
liveTerminal _ = pure False

-- | The size that the progress output uses: 80 columns by 24 rows.
terminalSize :: Handle -> IO (Int, Int)
terminalSize _ = pure (80, 24)
