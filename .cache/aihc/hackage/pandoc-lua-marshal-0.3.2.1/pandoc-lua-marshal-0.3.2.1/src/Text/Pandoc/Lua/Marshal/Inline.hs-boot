{-# LANGUAGE FlexibleContexts     #-}
module Text.Pandoc.Lua.Marshal.Inline
  ( -- * Single Inline elements
    peekInline
  , peekInlineFuzzy
  , pushInline
    -- * List of Inlines
  , peekInlines
  , peekInlinesFuzzy
  , pushInlines
    -- * Constructors
  , inlineConstructors
  , mkInlines
    -- * Walking
  , walkInlineSplicing
  , walkInlinesStraight
  ) where

import HsLua
import Text.Pandoc.Definition (Inline)
import Text.Pandoc.Lua.Marshal.Filter (Filter)
import Text.Pandoc.Lua.Walk (SpliceList, Walkable)

-- * Single Inline elements
peekInline       :: LuaError e => Peeker e Inline
peekInlineFuzzy  :: LuaError e => Peeker e Inline
pushInline       :: LuaError e => Pusher e Inline
-- * List of Inlines
peekInlines      :: LuaError e => Peeker e [Inline]
peekInlinesFuzzy :: LuaError e => Peeker e [Inline]
pushInlines      :: LuaError e => Pusher e [Inline]
-- * Constructors
inlineConstructors :: LuaError e => [DocumentedFunction e]
mkInlines          :: LuaError e => DocumentedFunction e
-- * Walking
walkInlineSplicing  :: (LuaError e, Walkable (SpliceList Inline) a)
                    => Filter -> a -> LuaE e a
walkInlinesStraight :: (LuaError e, Walkable [Inline] a)
                    => Filter -> a -> LuaE e a