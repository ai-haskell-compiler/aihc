{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
Copyright   : © 2021-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Definition and marshaling of the 'SimpleTable' data type used as a
convenience type when dealing with tables.
-}
module Text.Pandoc.Lua.Marshal.SimpleTable
  ( SimpleTable (..)
  , peekSimpleTable
  , pushSimpleTable
  , mkSimpleTable
  )
  where

import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import HsLua as Lua
import Text.Pandoc.Lua.Marshal.Alignment (peekAlignment, pushAlignment)
import Text.Pandoc.Lua.Marshal.Block (peekBlocksFuzzy, pushBlocks)
import Text.Pandoc.Lua.Marshal.Inline (peekInlinesFuzzy, pushInlines)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Definition
import qualified Data.Text as T

-- | A simple (legacy-style) table.
data SimpleTable = SimpleTable
  { simpleTableCaption :: [Inline]
  , simpleTableAlignments :: [Alignment]
  , simpleTableColumnWidths :: [Double]
  , simpleTableHeader :: [[Block]]
  , simpleTableBody :: [[[Block]]]
  } deriving stock (Eq, Show)

typeSimpleTable :: LuaError e => DocumentedType e SimpleTable
typeSimpleTable = deftype "SimpleTable"
  [ operation Eq $ lambda
    ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
    <#> parameter (optional . peekSimpleTable) "value" "a" ""
    <#> parameter (optional . peekSimpleTable) "value" "b" ""
    =#> functionResult pushBool "boolean" "whether the two objects are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeSimpleTable "self" ""
    =#> functionResult pushString "string" "Haskell string representation"
  ]
  [ property "caption" "table caption"
      (pushInlines, simpleTableCaption)
      (peekInlinesFuzzy, \t capt -> t {simpleTableCaption = capt})
  , property "aligns" "column alignments"
      (pushPandocList pushAlignment, simpleTableAlignments)
      (peekList peekAlignment, \t aligns -> t{simpleTableAlignments = aligns})
  , property "widths" "relative column widths"
      (pushPandocList pushRealFloat, simpleTableColumnWidths)
      (peekList peekRealFloat, \t ws -> t{simpleTableColumnWidths = ws})
  , property "headers" "table header"
      (pushRow, simpleTableHeader)
      (peekRow, \t h -> t{simpleTableHeader = h})
  , property "rows" "table body rows"
      (pushPandocList pushRow, simpleTableBody)
      (peekList peekRow, \t bs -> t{simpleTableBody = bs})

  , readonly "t" "type tag (always 'SimpleTable')"
      (pushText, const "SimpleTable")

  , alias "header" "alias for `headers`" ["headers"]
  ]
 where
  pushRow = pushPandocList pushBlocks

peekRow :: LuaError e => Peeker e [[Block]]
peekRow = peekList peekBlocksFuzzy

-- | Push a simple table to the stack by calling the
-- @pandoc.SimpleTable@ constructor.
pushSimpleTable :: forall e. LuaError e => SimpleTable -> LuaE e ()
pushSimpleTable = pushUD typeSimpleTable

-- | Retrieve a simple table from the stack.
peekSimpleTable :: forall e. LuaError e => Peeker e SimpleTable
peekSimpleTable = retrieving "SimpleTable" . peekUD typeSimpleTable

-- | Constructor for the 'SimpleTable' type.
mkSimpleTable :: LuaError e => DocumentedFunction e
mkSimpleTable = defun "SimpleTable"
  ### liftPure5 SimpleTable
  <#> parameter peekInlinesFuzzy "Inlines" "caption"
        "table caption"
  <#> parameter (peekList peekAlignment) "{Alignment,...}" "align"
        "column alignments"
  <#> parameter (peekList peekRealFloat) "{number,...}" "widths"
        "relative column widths"
  <#> parameter peekRow "{Blocks,...}" "header"
        "table header row"
  <#> parameter (peekList peekRow) "{{Blocks,...},...}" "rows"
        "table rows"
  =#> functionResult pushSimpleTable "SimpleTable" "new SimpleTable object"
  #? T.unlines
  [ "Usage:"
  , " "
  , "    local caption = \"Overview\""
  , "    local aligns = {pandoc.AlignDefault, pandoc.AlignDefault}"
  , "    local widths = {0, 0} -- let pandoc determine col widths"
  , "    local headers = {{pandoc.Plain({pandoc.Str \"Language\"})},"
  , "                     {pandoc.Plain({pandoc.Str \"Typing\"})}}"
  , "    local rows = {"
  , "      {{pandoc.Plain \"Haskell\"}, {pandoc.Plain \"static\"}},"
  , "      {{pandoc.Plain \"Lua\"}, {pandoc.Plain \"Dynamic\"}},"
  , "    }"
  , "    simple_table = pandoc.SimpleTable("
  , "      caption,"
  , "      aligns,"
  , "      widths,"
  , "      headers,"
  , "      rows"
  , "    )"
  ]
