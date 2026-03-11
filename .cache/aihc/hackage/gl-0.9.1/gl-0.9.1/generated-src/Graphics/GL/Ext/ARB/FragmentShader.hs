-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.FragmentShader (
  -- * Extension Support
    gl_ARB_fragment_shader

  -- * GL_ARB_fragment_shader
  , pattern GL_FRAGMENT_SHADER_ARB
  , pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB
  , pattern GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/fragment_shader.txt GL_ARB_fragment_shader> extension is available.

gl_ARB_fragment_shader :: Bool
gl_ARB_fragment_shader = member "GL_ARB_fragment_shader" extensions
{-# NOINLINE gl_ARB_fragment_shader #-}

pattern GL_FRAGMENT_SHADER_ARB  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_SHADER_ARB = 0x8B30

pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB = 0x8B8B

pattern GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB = 0x8B49