{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'Pandoc' values.
-}
module Text.Pandoc.Lua.Marshal.Pandoc
  ( -- * Pandoc
    typePandoc
  , peekPandoc
  , pushPandoc
  , mkPandoc
    -- * Meta
  , peekMeta
  , pushMeta
  , mkMeta
    -- * Filtering
  , applyFully
  ) where

import Control.Applicative (optional)
import Control.Monad ((<$!>))
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Definition (Pandoc (..), Meta (..), nullMeta)
import Text.Pandoc.Lua.Marshal.Block (peekBlocksFuzzy, pushBlocks)
import Text.Pandoc.Lua.Marshal.Filter
import Text.Pandoc.Lua.Marshal.MetaValue (peekMetaValue, pushMetaValue)
import Text.Pandoc.Lua.Marshal.Shared (walkBlocksAndInlines)
import Text.Pandoc.Lua.Walk (applyStraight)
import Text.Pandoc.Walk (Walkable (walk))
import qualified Text.Pandoc.Builder as B

-- | Pushes a 'Pandoc' value as userdata.
pushPandoc :: LuaError e => Pusher e Pandoc
pushPandoc = pushUD typePandoc

-- | Retrieves a 'Pandoc' document from a userdata value.
peekPandoc :: LuaError e => Peeker e Pandoc
peekPandoc = retrieving "Pandoc" . peekUD typePandoc

-- | Pandoc object type.
typePandoc :: LuaError e => DocumentedType e Pandoc
typePandoc = deftype "Pandoc"
  [ operation Concat $ lambda
     ### liftPure2 (<>)
     <#> parameter peekPandoc "Pandoc" "a" ""
     <#> parameter peekPandoc "Pandoc" "b" ""
     =#> functionResult pushPandoc "Pandoc" "combined documents"
  , operation Eq $ defun "__eq"
     ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
     <#> parameter (optional . peekPandoc) "doc1" "pandoc" ""
     <#> parameter (optional . peekPandoc) "doc2" "pandoc" ""
     =#> functionResult pushBool "boolean" "true iff the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekPandoc "Pandoc" "doc" ""
    =#> functionResult pushString "string" "native Haskell representation"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typePandoc "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ property "blocks" "list of blocks"
      (pushBlocks, \(Pandoc _ blks) -> blks)
      (peekBlocksFuzzy, \(Pandoc m _) blks -> Pandoc m blks)
  , property "meta" "document metadata"
      (pushMeta, \(Pandoc meta _) -> meta)
      (peekMeta, \(Pandoc _ blks) meta -> Pandoc meta blks)

  , method $ defun "clone"
      ### return
      <#> parameter peekPandoc "Pandoc" "doc" "self"
      =#> functionResult pushPandoc "Pandoc" "cloned Pandoc document"

  , method $ defun "normalize"
      ### liftPure normalize
      <#> udparam typePandoc "self" ""
      =#> udresult typePandoc "cloned and normalized document"
      #? "Perform a normalization of Pandoc documents. E.g., multiple\
          \ successive spaces are collapsed, and tables are normalized, so\
          \ that all rows and columns contain the same number of cells."

  , method $ defun "walk"
    ### flip applyFully
    <#> parameter peekPandoc "Pandoc" "self" ""
    <#> parameter peekFilter "Filter" "lua_filter" "table of filter functions"
    =#> functionResult pushPandoc "Pandoc" "modified element"
  ]

-- | Pushes a 'Meta' value as a string-indexed table.
pushMeta :: LuaError e => Pusher e Meta
pushMeta (Meta mmap) = do
  pushMap pushText pushMetaValue mmap
  _ <- newmetatable "Meta"
  setmetatable (nth 2)

-- | Retrieves a 'Meta' value from a string-indexed table.
peekMeta :: LuaError e => Peeker e Meta
peekMeta idx = retrieving "Meta" $
  Meta <$!> peekMap peekText peekMetaValue idx

-- | Constructor function for 'Pandoc' values.
mkPandoc :: LuaError e => DocumentedFunction e
mkPandoc = defun "Pandoc"
  ### liftPure2 (\blocks mMeta -> Pandoc (fromMaybe nullMeta mMeta) blocks)
  <#> parameter peekBlocksFuzzy "Blocks" "blocks" "document contents"
  <#> opt (parameter peekMeta "Meta" "meta" "document metadata")
  =#> functionResult pushPandoc "Pandoc" "new Pandoc document"

-- | Constructor for 'Meta' values.
mkMeta :: LuaError e => DocumentedFunction e
mkMeta = defun "Meta"
  ### liftPure id
  <#> parameter peekMeta "table" "meta" "table containing meta information"
  =#> functionResult pushMeta "table" "new Meta table"

-- | Applies a filter function to a Pandoc value.
applyPandocFunction :: LuaError e
                          => Filter
                          -> Pandoc -> LuaE e Pandoc
applyPandocFunction = applyStraight pushPandoc peekPandoc

-- | Applies a filter function to a Meta value.
applyMetaFunction :: LuaError e
                        => Filter
                        -> Pandoc -> LuaE e Pandoc
applyMetaFunction filter' (Pandoc meta blocks) = do
  meta' <- applyStraight pushMeta peekMeta filter' meta
  pure (Pandoc meta' blocks)

-- | Apply all components of a Lua filter.
--
-- These operations are run in order:
--
-- - Inline filter functions are applied to Inline elements, splicing
--   the result back into the list of Inline elements
--
-- - The @Inlines@ function is applied to all lists of Inlines.
--
-- - Block filter functions are applied to Block elements, splicing the
--   result back into the list of Block elements
--
-- - The @Blocks@ function is applied to all lists of Blocks.
--
-- - The @Meta@ function is applied to the 'Meta' part.
--
-- - The @Pandoc@ function is applied to the full 'Pandoc' element.
applyFully :: LuaError e
           => Filter
           -> Pandoc -> LuaE e Pandoc
applyFully filter' doc = case filterWalkingOrder filter' of
  WalkForEachType -> walkBlocksAndInlines filter' doc
                 >>= applyMetaFunction filter'
                 >>= applyPandocFunction filter'
  WalkTopdown     -> applyPandocFunction filter' doc
                 >>= applyMetaFunction filter'
                 >>= walkBlocksAndInlines filter'

-- | Normalize a document
normalize :: (Walkable [B.Inline] a, Walkable B.Block a) => a -> a
normalize =
  let normalizeBlock = \case
        B.Table attr capt specs th tbs tf ->
          let twidth = length specs
              th'  = B.normalizeTableHead twidth th
              tbs' = map (B.normalizeTableBody twidth) tbs
              tf'  = B.normalizeTableFoot twidth tf
          in B.Table attr capt specs th' tbs' tf'
        x -> x
  in walk normalizeBlock .
     walk (B.toList . mconcat . map (B.singleton @B.Inline))
