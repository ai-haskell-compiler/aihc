{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Copyright  : © 2021-2026 Albert Krewinkel
License    : MIT
Maintainer : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Lua lists with additional methods.
-}
module Text.Pandoc.Lua.Marshal.List
  ( module HsLua.List
  , pushPandocList
  ) where

import HsLua
import HsLua.List

-- | Pushes a list as a numerically-indexed Lua table, and sets a
-- metatable that offers a number of convenience functions.
pushPandocList :: LuaError e => Pusher e a -> Pusher e [a]
pushPandocList pushItem items = do
  pushList pushItem items
  getmetatable' "List" >>= \case
    TypeTable -> setmetatable (nth 2)
    _ -> failLua "List has not been initialized correctly."
