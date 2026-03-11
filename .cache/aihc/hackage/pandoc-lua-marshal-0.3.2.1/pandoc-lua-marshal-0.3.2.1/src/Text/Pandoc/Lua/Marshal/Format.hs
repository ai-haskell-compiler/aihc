{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'Format' values.
-}
module Text.Pandoc.Lua.Marshal.Format
  ( peekFormat
  , pushFormat
  ) where

import Control.Monad ((<$!>))
import HsLua
import Text.Pandoc.Definition (Format (Format))

-- | Retrieves a 'Format' value from a string.
peekFormat :: Peeker e Format
peekFormat idx = Format <$!> peekText idx

-- | Pushes a 'Format' value as a string.
pushFormat :: Pusher e Format
pushFormat (Format f) = pushText f
