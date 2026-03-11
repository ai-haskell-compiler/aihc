{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase           #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'Row' values.
-}
module Text.Pandoc.Lua.Marshal.Row
  ( peekRow
  , peekRowFuzzy
  , pushRow
  , typeRow
  , mkRow
  ) where

import Control.Applicative (optional)
import Control.Monad ((<$!>))
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import Text.Pandoc.Lua.Marshal.Cell (peekCellFuzzy, pushCell)
import Text.Pandoc.Lua.Marshal.Filter (peekFilter)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Marshal.Shared (walkBlocksAndInlines)
import Text.Pandoc.Definition

-- | Push a table Row as a table with fields @attr@, @alignment@,
-- @row_span@, @col_span@, and @contents@.
pushRow :: LuaError e => Row -> LuaE e ()
pushRow = pushUD typeRow

-- | Retrieves a 'Cell' object from the stack.
peekRow :: LuaError e => Peeker e Row
peekRow = peekUD typeRow

-- | Retrieves a 'Cell' from the stack, accepting either a 'pandoc Cell'
-- userdata object or a table with fields @attr@, @alignment@, @row_span@,
-- @col_span@, and @contents@.
peekRowFuzzy :: LuaError e => Peeker e Row
peekRowFuzzy idx = liftLua (ltype idx) >>= \case
  TypeUserdata -> peekRow idx
  TypeTable -> uncurry Row <$!> peekPair peekAttr (peekList peekCellFuzzy) idx
  _ -> failPeek =<< typeMismatchMessage "Cell or table" idx

-- | Row object type.
typeRow :: LuaError e => DocumentedType e Row
typeRow = deftype "Row"
  [ operation Eq $ defun "__eq"
     ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
     <#> parameter (optional . peekRow) "Row" "self" ""
     <#> parameter (optional . peekRow) "any" "object" ""
     =#> functionResult pushBool "boolean" "true iff the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekRow "Row" "self" ""
    =#> functionResult pushString "string" "native Haskell representation"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeRow "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ property "attr" "row attributes"
      (pushAttr, \(Row attr _) -> attr)
      (peekAttr, \(Row _ cells) attr ->
                   Row attr cells)
  , property "cells" "row cells"
      (pushPandocList pushCell, \(Row _ cells) -> cells)
      (peekList peekCellFuzzy, \(Row attr _) cells ->
                                 Row attr cells)

  , alias "identifier" "cell ID"         ["attr", "identifier"]
  , alias "classes"    "cell classes"    ["attr", "classes"]
  , alias "attributes" "cell attributes" ["attr", "attributes"]

  , method $ defun "clone"
    ### return
    <#> parameter peekRow "Row" "self" ""
    =#> functionResult pushRow "Row" "cloned object"

  , method $ defun "walk"
    ### flip walkBlocksAndInlines
    <#> parameter peekRow "Row" "self" ""
    <#> parameter peekFilter "Filter" "lua_filter" "table of filter functions"
    =#> functionResult pushRow "Row" "modified cell"
  ]

-- | Constructor function for 'Row' values.
mkRow :: LuaError e => DocumentedFunction e
mkRow = defun "Row"
  ### liftPure2 (\mCells mAttr -> Row
                  (fromMaybe nullAttr mAttr)
                  (fromMaybe [] mCells))
  <#> opt (parameter (peekList peekCellFuzzy) "{Cell,...}" "cells"
           "list of table cells in this row")
  <#> opt (parameter peekAttr "Attr" "attr" "row attributes")
  =#> functionResult pushRow "Row" "new Row object"
  #? "Creates a table row."
