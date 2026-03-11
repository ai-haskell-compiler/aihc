-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.BaseInstance (
  -- * Extension Support
    gl_ARB_base_instance

  -- * GL_ARB_base_instance
  , glDrawArraysInstancedBaseInstance
  , glDrawElementsInstancedBaseInstance
  , glDrawElementsInstancedBaseVertexBaseInstance
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/base_instance.txt GL_ARB_base_instance> extension is available.

gl_ARB_base_instance :: Bool
gl_ARB_base_instance = member "GL_ARB_base_instance" extensions
{-# NOINLINE gl_ARB_base_instance #-}