-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TimerQuery (
  -- * Extension Support
    gl_EXT_timer_query

  -- * GL_EXT_timer_query
  , glGetQueryObjecti64vEXT
  , glGetQueryObjectui64vEXT
  , pattern GL_TIME_ELAPSED_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/timer_query.txt GL_EXT_timer_query> extension is available.

gl_EXT_timer_query :: Bool
gl_EXT_timer_query = member "GL_EXT_timer_query" extensions
{-# NOINLINE gl_EXT_timer_query #-}