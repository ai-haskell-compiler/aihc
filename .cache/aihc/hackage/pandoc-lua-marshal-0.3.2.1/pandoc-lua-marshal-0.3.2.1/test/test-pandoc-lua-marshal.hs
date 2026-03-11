{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : Main
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the pandoc types handling in Lua.
-}
module Main (main) where

import Control.Monad (forM_, when)
import Data.Data (Data, dataTypeConstrs, dataTypeOf, showConstr)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import HsLua as Lua
import Test.Tasty.QuickCheck (ioProperty, testProperty)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Lua (translateResultsFromFile)
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Marshal.AST

main :: IO ()
main = do
  listAttributeTests <- run @Lua.Exception $ do
    openlibs
    register' mkListAttributes
    registerConstants (Proxy @ListNumberStyle)
    registerConstants (Proxy @ListNumberDelim)
    translateResultsFromFile "test/test-listattributes.lua"

  attrTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    register' mkAttr
    register' mkAttributeList
    translateResultsFromFile "test/test-attr.lua"

  citationTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    register' mkCitation
    registerConstants (Proxy @CitationMode)
    forM_ inlineConstructors register'
    translateResultsFromFile "test/test-citation.lua"

  inlineTests <- run @Lua.Exception $ do
    registerDefault
    translateResultsFromFile "test/test-inline.lua"

  blockTests <- run @Lua.Exception $ do
    registerDefault
    register' mkCaption
    translateResultsFromFile "test/test-block.lua"

  cellTests <- run @Lua.Exception $ do
    registerDefault
    translateResultsFromFile "test/test-cell.lua"

  simpleTableTests <- run @Lua.Exception $ do
    registerDefault
    translateResultsFromFile "test/test-simpletable.lua"

  metavalueTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    forM_ metaValueConstructors register'
    translateResultsFromFile "test/test-metavalue.lua"

  pandocTests <- run @Lua.Exception $ do
    registerDefault
    translateResultsFromFile "test/test-pandoc.lua"

  tableTests <- run @Lua.Exception $ do
    registerDefault
    translateResultsFromFile "test/test-table.lua"

  defaultMain $ testGroup "pandoc-lua-marshal"
    [ roundtrips
    , listAttributeTests
    , attrTests
    , citationTests
    , inlineTests
    , blockTests
    , cellTests
    , simpleTableTests
    , metavalueTests
    , pandocTests
    , tableTests
    ]

-- | Registers all constructors and string constants in the global
-- environment.
registerDefault :: LuaError e => LuaE e ()
registerDefault = do
  openlibs
  pushListModule *> setglobal "List"
  register' mkAttr
  register' mkBlocks
  register' mkCell
  register' mkCitation
  register' mkInlines
  register' mkListAttributes
  register' mkPandoc
  register' mkRow
  register' mkSimpleTable
  register' mkTableBody
  register' mkTableHead
  register' mkTableFoot
  registerConstants (Proxy @Alignment)
  registerConstants (Proxy @ListNumberStyle)
  registerConstants (Proxy @ListNumberStyle)
  registerConstants (Proxy @MathType)
  registerConstants (Proxy @QuoteType)
  forM_ inlineConstructors register'
  forM_ blockConstructors register'

register' :: LuaError e => DocumentedFunction e -> LuaE e ()
register' f = do
  pushDocumentedFunction f
  setglobal (functionName f)

registerConstants :: forall a e. (Data a, LuaError e) => Proxy a -> LuaE e ()
registerConstants proxy =
  forM_ (constructors proxy) $ \c -> do
    pushString c
    setglobal (fromString c)

constructors :: forall a. Data a => Proxy a -> [String]
constructors _ = map showConstr . dataTypeConstrs . dataTypeOf @a $ undefined

--
-- Roundtrips
--

-- | Basic tests
roundtrips :: TestTree
roundtrips = testGroup "Roundtrip through Lua stack"
  [ testProperty "Alignment" $
    ioProperty . roundtripEqual pushAlignment peekAlignment

  , testProperty "Block" $
    ioProperty . roundtripEqual pushBlock peekBlockFuzzy

  , testProperty "[Block]" $
    ioProperty . roundtripEqual pushBlocks peekBlocksFuzzy

  , testProperty "Caption" $
    ioProperty . roundtripEqual pushCaption peekCaption

  , testProperty "Cell" $
    ioProperty . roundtripEqual pushCell peekCell

  , testProperty "Citation" $
    ioProperty . roundtripEqual pushCitation peekCitation

  , testProperty "CitationMode" $
    ioProperty . roundtripEqual pushCitationMode peekCitationMode

  , testProperty "Inline" $
    ioProperty . roundtripEqual pushInline peekInlineFuzzy

  , testProperty "[Inline]" $
    ioProperty . roundtripEqual pushInlines peekInlinesFuzzy

  , testProperty "ListNumberStyle" $
    ioProperty . roundtripEqual pushListNumberStyle peekListNumberStyle

  , testProperty "ListNumberDelim" $
    ioProperty . roundtripEqual pushListNumberDelim peekListNumberDelim

  , testProperty "MathType" $
    ioProperty . roundtripEqual pushMathType peekMathType

  , testProperty "Meta" $
    ioProperty . roundtripEqual pushMeta peekMeta

  , testProperty "Pandoc" $
    ioProperty . roundtripEqual pushPandoc peekPandoc

  , testProperty "Row" $
    ioProperty . roundtripEqual pushRow peekRow

  , testProperty "QuoteType" $
    ioProperty . roundtripEqual pushQuoteType peekQuoteType

  , testProperty "TableBody" $
    ioProperty . roundtripEqual pushTableBody peekTableBodyFuzzy

  , testProperty "TableHead" $
    ioProperty . roundtripEqual pushTableHead peekTableHead
  ]

roundtripEqual :: forall a. Eq a
               => Pusher Lua.Exception a -> Peeker Lua.Exception a
               -> a -> IO Bool
roundtripEqual pushX peekX x = (x ==) <$> roundtripped
 where
  roundtripped :: IO a
  roundtripped = run $ do
    openlibs
    pushListModule <* pop 1
    oldSize <- gettop
    pushX x
    size <- gettop
    when (size - oldSize /= 1) $
      Prelude.error ("Only one value should have been pushed" ++ show size)
    forcePeek $ peekX top
