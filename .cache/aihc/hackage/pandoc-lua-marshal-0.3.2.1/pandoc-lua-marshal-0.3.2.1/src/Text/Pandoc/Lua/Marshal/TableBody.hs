{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase           #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel,
                          © 2025-2026 Sean Soon
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'TableBody' values.
-}
module Text.Pandoc.Lua.Marshal.TableBody
  ( peekTableBody
  , peekTableBodyFuzzy
  , pushTableBody
  , typeTableBody
  , mkTableBody
  ) where

import Control.Applicative (optional)
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Marshal.Row (peekRowFuzzy, pushRow)
import Text.Pandoc.Definition

-- | Push a TableBody as a userdata value.
pushTableBody :: LuaError e => TableBody -> LuaE e ()
pushTableBody = pushUD typeTableBody

-- | Retrieves a TableBody from the stack.
peekTableBody :: LuaError e => Peeker e TableBody
peekTableBody = peekUD typeTableBody

-- | Retrieves a TableBody from the stack, accepting either a 
-- 'pandoc TableBody' userdata object or a table with fields @attr@,
-- @body@, @head@, @row_head_columns@.
peekTableBodyFuzzy :: LuaError e => Peeker e TableBody
peekTableBodyFuzzy idx = retrieving "TableBody" $ liftLua (ltype idx) >>= \case
  TypeUserdata -> peekTableBody idx
  TypeTable -> do
    attr <- peekFieldRaw peekAttr "attr" idx
    body <- peekFieldRaw (peekList peekRowFuzzy) "body" idx
    ihead <- peekFieldRaw (peekList peekRowFuzzy) "head" idx
    rhc <- peekFieldRaw (fmap RowHeadColumns . peekIntegral) "row_head_columns" idx
    return $! TableBody attr rhc ihead body
  _ -> failPeek =<< typeMismatchMessage "Cell or table" idx

-- | TableBody object type.
typeTableBody :: LuaError e => DocumentedType e TableBody
typeTableBody = deftype "TableBody"
  [ operation Eq $ defun "__eq"
     ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
     <#> parameter (optional . peekTableBody) "TableBody" "self" ""
     <#> parameter (optional . peekTableBody) "any" "object" ""
     =#> functionResult pushBool "boolean" "true iff the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekTableBody "TableBody" "self" ""
    =#> functionResult pushString "string" "native Haskell representation"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeTableBody "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ property "attr" "table body attributes"
      (pushAttr, \(TableBody attr _ _ _) -> attr)
      (peekAttr, \(TableBody _ body ihead rhc) attr ->
                   TableBody attr body ihead rhc)
  , property "row_head_columns" "number of columns taken up by the row head of each row of the TableBody"
    (pushIntegral, \(TableBody _ (RowHeadColumns rhc) _ _) -> rhc)
    (peekIntegral, \(TableBody attr _ ihead body) rhc ->
                      TableBody attr (RowHeadColumns rhc) ihead body)
  , property "head" "intermediate head"
      (pushPandocList pushRow, \(TableBody _ _ ihead _) -> ihead)
      (peekList peekRowFuzzy, \(TableBody attr rhc _ body) ihead ->
                                TableBody attr rhc ihead body)
  , property "body" "table body rows"
      (pushPandocList pushRow, \(TableBody _ _ _ body) -> body)
      (peekList peekRowFuzzy, \(TableBody attr rhc ihead _) body ->
                                TableBody attr rhc ihead body)
  , alias "identifier" "cell ID"         ["attr", "identifier"]
  , alias "classes"    "cell classes"    ["attr", "classes"]
  , alias "attributes" "cell attributes" ["attr", "attributes"]

  , method $ defun "clone"
    ### return
    <#> parameter peekTableBody "TableBody" "self" ""
    =#> functionResult pushTableBody "TableBody" "cloned object"
  ]

-- | Constructor function for 'TableBody' values.
mkTableBody :: LuaError e => DocumentedFunction e
mkTableBody = defun "TableBody"
  ### liftPure4 (\mBody mHead mRhc mAttr -> TableBody
                  (fromMaybe nullAttr mAttr)
                  (maybe 0 RowHeadColumns mRhc)
                  (fromMaybe [] mHead)
                  (fromMaybe [] mBody))
  <#> opt (parameter (peekList peekRowFuzzy) "{Row,...}" "body"
           "list of table rows")
  <#> opt (parameter (peekList peekRowFuzzy) "{Row,...}" "head"
           "intermediate head")
  <#> opt (parameter peekIntegral "integer" "row_head_columns"
           "number of columns taken up by the row head of each row of the TableBody")
  <#> opt (parameter peekAttr "Attr" "attr" "table body attributes")
  =#> functionResult pushTableBody "TableBody" "new TableBody object"
  #? "Creates a table body."
