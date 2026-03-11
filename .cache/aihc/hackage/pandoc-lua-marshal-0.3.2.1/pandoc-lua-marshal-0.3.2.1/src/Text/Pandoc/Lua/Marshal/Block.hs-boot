{-# LANGUAGE FlexibleContexts     #-}
module Text.Pandoc.Lua.Marshal.Block
  ( -- * Single Block elements
    peekBlock
  , peekBlockFuzzy
  , pushBlock
    -- * List of Blocks
  , peekBlocks
  , peekBlocksFuzzy
  , pushBlocks
    -- * Constructors
  , blockConstructors
  , mkBlocks
    -- * Walk
  , walkBlockSplicing
  , walkBlocksStraight
  ) where

import HsLua
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Marshal.Filter (Filter)
import Text.Pandoc.Lua.Walk (SpliceList, Walkable)

-- * Single Block elements
peekBlock      :: LuaError e => Peeker e Block
peekBlockFuzzy :: LuaError e => Peeker e Block
pushBlock      :: LuaError e => Pusher e Block
-- * List of Blocks
peekBlocks      :: LuaError e => Peeker e [Block]
peekBlocksFuzzy :: LuaError e => Peeker e [Block]
pushBlocks      :: LuaError e => Pusher e [Block]
-- * Constructors
blockConstructors :: LuaError e => [DocumentedFunction e]
mkBlocks          :: LuaError e => DocumentedFunction e
-- * Walk
walkBlockSplicing  :: (LuaError e, Walkable (SpliceList Block) a)
                   => Filter -> a -> LuaE e a
walkBlocksStraight :: (LuaError e, Walkable [Block] a)
                   => Filter -> a -> LuaE e a