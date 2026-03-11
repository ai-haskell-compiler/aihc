{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright   : © 2021-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Shared functions used in multiple types.
-}
module Text.Pandoc.Lua.Marshal.Shared
  ( -- * Walking
    walkBlocksAndInlines
  ) where

import Prelude hiding (lookup)
import Control.Monad ((>=>))
import HsLua
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Block
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Inline
import Text.Pandoc.Lua.Marshal.Filter
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Topdown
import Text.Pandoc.Lua.Walk
import Text.Pandoc.Walk

-- | Walk blocks and inlines.
walkBlocksAndInlines :: (LuaError e,
                         Walkable (SpliceList Block) a,
                         Walkable (SpliceList Inline) a,
                         Walkable [Block] a,
                         Walkable [Inline] a,
                         Walkable Topdown a)
                     => Filter
                     -> a -> LuaE e a
walkBlocksAndInlines filter' =
  case filterWalkingOrder filter' of
    WalkTopdown     -> walkM (applyFilterTopdown filter')
    WalkForEachType -> walkInlineSplicing filter'
                   >=> walkInlinesStraight filter'
                   >=> walkBlockSplicing filter'
                   >=> walkBlocksStraight filter'

-- | Applies a filter by processing the root node(s) first and descending
-- towards the leaves depth-first.
applyFilterTopdown :: LuaError e
                   => Filter
                   -> Topdown -> LuaE e Topdown
applyFilterTopdown filter' topdown@(Topdown _ node) =
  case node of
    TBlock x ->
      case filter' `getFunctionFor` x of
        Nothing ->
          pure topdown
        Just fn -> do
          (blocks, ctrl) <-
            applySplicingFunction fn pushBlock peekBlocksFuzzy x
          pure $ Topdown ctrl $ TBlocks blocks

    TBlocks xs ->
      case "Blocks" `lookup` filter' of
        Nothing ->
          pure topdown
        Just fn -> do
          (blocks, ctrl) <-
            applyStraightFunction fn pushBlocks peekBlocksFuzzy xs
          pure $ Topdown ctrl $ TBlocks blocks

    TInline x ->
      case filter' `getFunctionFor` x of
        Nothing ->
          pure topdown
        Just fn -> do
          (inlines, ctrl) <-
            applySplicingFunction fn pushInline peekInlinesFuzzy x
          pure $ Topdown ctrl $ TInlines inlines

    TInlines xs ->
      case "Inlines" `lookup` filter' of
        Nothing ->
          pure topdown
        Just fn -> do
          (inlines, ctrl) <-
            applyStraightFunction fn pushInlines peekInlinesFuzzy xs
          pure $ Topdown ctrl $ TInlines inlines
