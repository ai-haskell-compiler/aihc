-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ReadBufferFront (
  -- * Extension Support
    gl_NV_read_buffer_front
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_NV_read_buffer_front extension is available.

gl_NV_read_buffer_front :: Bool
gl_NV_read_buffer_front = member "GL_NV_read_buffer_front" extensions
{-# NOINLINE gl_NV_read_buffer_front #-}