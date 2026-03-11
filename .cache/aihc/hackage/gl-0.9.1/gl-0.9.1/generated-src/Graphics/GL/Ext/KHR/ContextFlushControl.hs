-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.ContextFlushControl (
  -- * Extension Support
    gl_KHR_context_flush_control

  -- * GL_KHR_context_flush_control
  , pattern GL_CONTEXT_RELEASE_BEHAVIOR
  , pattern GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH
  , pattern GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_KHR
  , pattern GL_CONTEXT_RELEASE_BEHAVIOR_KHR
  , pattern GL_NONE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/KHR/context_flush_control.txt GL_KHR_context_flush_control> extension is available.

gl_KHR_context_flush_control :: Bool
gl_KHR_context_flush_control = member "GL_KHR_context_flush_control" extensions
{-# NOINLINE gl_KHR_context_flush_control #-}

pattern GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_KHR  :: (Eq a, Num a) => a

pattern GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_KHR = 0x82FC

pattern GL_CONTEXT_RELEASE_BEHAVIOR_KHR  :: (Eq a, Num a) => a

pattern GL_CONTEXT_RELEASE_BEHAVIOR_KHR = 0x82FB