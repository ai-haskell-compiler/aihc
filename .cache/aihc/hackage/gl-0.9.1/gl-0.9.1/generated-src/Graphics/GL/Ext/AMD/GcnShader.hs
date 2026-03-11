-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.GcnShader (
  -- * Extension Support
    gl_AMD_gcn_shader
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/gcn_shader.txt GL_AMD_gcn_shader> extension is available.

gl_AMD_gcn_shader :: Bool
gl_AMD_gcn_shader = member "GL_AMD_gcn_shader" extensions
{-# NOINLINE gl_AMD_gcn_shader #-}