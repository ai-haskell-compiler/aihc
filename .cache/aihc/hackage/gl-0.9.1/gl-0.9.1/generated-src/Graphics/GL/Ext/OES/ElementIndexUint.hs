-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.ElementIndexUint (
  -- * Extension Support
    gl_OES_element_index_uint

  -- * GL_OES_element_index_uint
  , pattern GL_UNSIGNED_INT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_element_index_uint.txt GL_OES_element_index_uint> extension is available.

gl_OES_element_index_uint :: Bool
gl_OES_element_index_uint = member "GL_OES_element_index_uint" extensions
{-# NOINLINE gl_OES_element_index_uint #-}