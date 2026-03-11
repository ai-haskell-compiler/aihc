{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'CitationMode' values.
-}
module Text.Pandoc.Lua.Marshal.CitationMode
  ( peekCitationMode
  , pushCitationMode
  ) where

import HsLua
import Text.Pandoc.Definition (CitationMode)

-- | Retrieves a Citation value from a string.
peekCitationMode :: Peeker e CitationMode
peekCitationMode = peekRead
{-# INLINE peekCitationMode #-}

-- | Pushes a CitationMode value as string.
pushCitationMode :: Pusher e CitationMode
pushCitationMode = pushString . show
{-# INLINE pushCitationMode #-}
