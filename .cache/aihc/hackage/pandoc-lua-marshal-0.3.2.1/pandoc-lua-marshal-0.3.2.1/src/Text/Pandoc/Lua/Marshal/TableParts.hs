{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of types that are used exclusively
with tables.
-}
module Text.Pandoc.Lua.Marshal.TableParts
  ( peekColSpec
  , pushColSpec
  , peekRow
  , peekRowFuzzy
  , pushRow
  , peekTableBody
  , peekTableBodyFuzzy
  , pushTableBody
  , peekTableFoot
  , pushTableFoot
  , peekTableHead
  , pushTableHead
    -- * Constructors
  , mkRow
  , mkTableBody
  , mkTableFoot
  , mkTableHead
  ) where

import Control.Applicative (optional)
import Control.Monad ((<$!>))
import HsLua
import Text.Pandoc.Lua.Marshal.Alignment (peekAlignment, pushAlignment)
import Text.Pandoc.Lua.Marshal.Row
import Text.Pandoc.Lua.Marshal.TableBody
import Text.Pandoc.Lua.Marshal.TableFoot
import Text.Pandoc.Lua.Marshal.TableHead
import Text.Pandoc.Definition

-- | Push a ColSpec value as a pair of Alignment and ColWidth.
pushColSpec :: LuaError e => Pusher e ColSpec
pushColSpec = pushPair pushAlignment pushColWidth

-- | Peek a ColSpec value as a pair of Alignment and ColWidth.
peekColSpec :: LuaError e => Peeker e ColSpec
peekColSpec = peekPair peekAlignment peekColWidth

peekColWidth :: Peeker e ColWidth
peekColWidth = retrieving "ColWidth" . \idx -> do
  maybe ColWidthDefault ColWidth <$!> optional (peekRealFloat idx)

-- | Push a ColWidth value by pushing the width as a plain number, or
-- @nil@ for ColWidthDefault.
pushColWidth :: LuaError e => Pusher e ColWidth
pushColWidth = \case
  (ColWidth w)    -> push w
  ColWidthDefault -> pushnil
