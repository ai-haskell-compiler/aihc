-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.ShaderTrinaryMinmax (
  -- * Extension Support
    gl_AMD_shader_trinary_minmax
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/shader_trinary_minmax.txt GL_AMD_shader_trinary_minmax> extension is available.

gl_AMD_shader_trinary_minmax :: Bool
gl_AMD_shader_trinary_minmax = member "GL_AMD_shader_trinary_minmax" extensions
{-# NOINLINE gl_AMD_shader_trinary_minmax #-}