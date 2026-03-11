{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'Alignment' values.
-}
module Text.Pandoc.Lua.Marshal.Alignment
  ( peekAlignment
  , pushAlignment
  ) where

import HsLua
import Text.Pandoc.Definition (Alignment)

-- | Retrieves a 'Alignment' value from a string.
peekAlignment :: Peeker e Alignment
peekAlignment = peekRead

-- | Pushes a 'Alignment' value as a string.
pushAlignment :: Pusher e Alignment
pushAlignment = pushString . show
