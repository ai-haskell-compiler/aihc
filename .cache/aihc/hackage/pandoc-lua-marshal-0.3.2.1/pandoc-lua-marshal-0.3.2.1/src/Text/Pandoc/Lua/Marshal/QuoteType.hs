{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'QuoteType' values.
-}
module Text.Pandoc.Lua.Marshal.QuoteType
  ( peekQuoteType
  , pushQuoteType
  ) where

import HsLua
import Text.Pandoc.Definition (QuoteType)

-- | Retrieves a 'QuoteType' value from a string.
peekQuoteType :: Peeker e QuoteType
peekQuoteType = peekRead

-- | Pushes a 'QuoteType' value as a string.
pushQuoteType :: Pusher e QuoteType
pushQuoteType = pushString . show
