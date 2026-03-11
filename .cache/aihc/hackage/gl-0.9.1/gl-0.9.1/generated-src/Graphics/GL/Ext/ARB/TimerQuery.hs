-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TimerQuery (
  -- * Extension Support
    gl_ARB_timer_query

  -- * GL_ARB_timer_query
  , glGetQueryObjecti64v
  , glGetQueryObjectui64v
  , glQueryCounter
  , pattern GL_TIMESTAMP
  , pattern GL_TIME_ELAPSED
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/timer_query.txt GL_ARB_timer_query> extension is available.

gl_ARB_timer_query :: Bool
gl_ARB_timer_query = member "GL_ARB_timer_query" extensions
{-# NOINLINE gl_ARB_timer_query #-}