{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase           #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of table 'Cell' values.
-}
module Text.Pandoc.Lua.Marshal.Cell
  ( peekCell
  , peekCellFuzzy
  , pushCell
  , typeCell
  , mkCell
  ) where

import Control.Applicative (optional)
import Control.Monad ((<$!>))
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Lua.Marshal.Alignment (peekAlignment, pushAlignment)
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Block
  ( peekBlocksFuzzy, pushBlocks )
import Text.Pandoc.Lua.Marshal.Filter (peekFilter)
import Text.Pandoc.Lua.Marshal.Shared (walkBlocksAndInlines)
import Text.Pandoc.Definition

-- | Push a table cell as a table with fields @attr@, @alignment@,
-- @row_span@, @col_span@, and @contents@.
pushCell :: LuaError e => Cell -> LuaE e ()
pushCell = pushUD typeCell

-- | Retrieves a 'Cell' object from the stack.
peekCell :: LuaError e => Peeker e Cell
peekCell = peekUD typeCell

-- | Retrieves a 'Cell' from the stack, accepting either a 'pandoc Cell'
-- userdata object or a table with fields @attr@, @alignment@, @row_span@,
-- @col_span@, and @contents@.
peekCellFuzzy :: LuaError e => Peeker e Cell
peekCellFuzzy idx = liftLua (ltype idx) >>= \case
  TypeUserdata -> peekCell idx
  TypeTable -> do
    attr <- peekFieldRaw peekAttr "attr" idx
    algn <- peekFieldRaw peekAlignment "alignment" idx
    rs   <- RowSpan <$!> peekFieldRaw peekIntegral "row_span" idx
    cs   <- ColSpan <$!> peekFieldRaw peekIntegral "col_span" idx
    blks <- peekFieldRaw peekBlocksFuzzy "contents" idx
    return $! Cell attr algn rs cs blks
  _ -> failPeek =<< typeMismatchMessage "Cell or table" idx

-- | Cell object type.
typeCell :: LuaError e => DocumentedType e Cell
typeCell = deftype "Cell"
  [ operation Eq $ defun "__eq"
     ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
     <#> parameter (optional . peekCell) "Cell" "self" ""
     <#> parameter (optional . peekCell) "any" "object" ""
     =#> functionResult pushBool "boolean" "true iff the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekCell "Cell" "self" ""
    =#> functionResult pushString "string" "native Haskell representation"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeCell "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ property "attr" "cell attributes"
      (pushAttr, \(Cell attr _ _ _ _) -> attr)
      (peekAttr, \(Cell _ align rs cs blks) attr ->
                   Cell attr align rs cs blks)
  , property "alignment" "alignment of cell contents"
      (pushAlignment, \(Cell _ align _ _ _) -> align)
      (peekAlignment, \(Cell attr _ rs cs blks) align ->
                        Cell attr align rs cs blks)
  , property "row_span" "number of rows over which this cell spans"
      (pushIntegral, \(Cell _ _ (RowSpan rs) _ _) -> rs)
      (peekIntegral, \(Cell attr align _ cs blks) rs ->
                       Cell attr align (RowSpan rs) cs blks)
  , property "col_span" "number of columns over which this cell spans"
      (pushIntegral, \(Cell _ _ _ (ColSpan rs) _) -> rs)
      (peekIntegral, \(Cell attr align rs _ blks) cs ->
                       Cell attr align rs (ColSpan cs) blks)
  , property "contents" "cell contents"
      (pushBlocks, \(Cell _ _ _ _ blks) -> blks)
      (peekBlocksFuzzy, \(Cell attr align rs cs _) blks ->
                          Cell attr align rs cs blks)

  , alias "content"    "alias for contents" ["contents"]
  , alias "identifier" "cell ID"         ["attr", "identifier"]
  , alias "classes"    "cell classes"    ["attr", "classes"]
  , alias "attributes" "cell attributes" ["attr", "attributes"]

  , method $ defun "walk"
    ### flip walkBlocksAndInlines
    <#> parameter peekCell "Cell" "self" ""
    <#> parameter peekFilter "Filter" "lua_filter" "table of filter functions"
    =#> functionResult pushCell "Cell" "modified cell"
  ]

-- | Constructor function for 'Cell' values.
mkCell :: LuaError e => DocumentedFunction e
mkCell = defun "Cell"
  ### liftPure5 (\blocks mAlign mRowSpan mColSpan mAttr -> Cell
                  (fromMaybe nullAttr mAttr)
                  (fromMaybe AlignDefault mAlign)
                  (maybe 1 RowSpan mRowSpan)
                  (maybe 1 ColSpan mColSpan)
                  blocks)
  <#> parameter peekBlocksFuzzy "Blocks" "blocks" "cell contents"
  <#> opt (parameter peekAlignment "Alignment" "align"
           "text alignment; defaults to `AlignDefault`")
  <#> opt (parameter peekIntegral "integer" "rowspan"
           "number of rows occupied by the cell; defaults to `1`")
  <#> opt (parameter peekIntegral "integer" "colspan"
           "number of columns occupied by the cell; defaults to `1`")
  <#> opt (parameter peekAttr "Attr" "attr" "cell attributes")
  =#> functionResult pushCell "Cell" "new Cell object"
  #? "Create a new table cell."
