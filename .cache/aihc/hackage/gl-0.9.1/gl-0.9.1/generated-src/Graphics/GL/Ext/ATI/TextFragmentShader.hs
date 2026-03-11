-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.TextFragmentShader (
  -- * Extension Support
    gl_ATI_text_fragment_shader

  -- * GL_ATI_text_fragment_shader
  , pattern GL_TEXT_FRAGMENT_SHADER_ATI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/text_fragment_shader.txt GL_ATI_text_fragment_shader> extension is available.

gl_ATI_text_fragment_shader :: Bool
gl_ATI_text_fragment_shader = member "GL_ATI_text_fragment_shader" extensions
{-# NOINLINE gl_ATI_text_fragment_shader #-}

pattern GL_TEXT_FRAGMENT_SHADER_ATI  :: (Eq a, Num a) => a

pattern GL_TEXT_FRAGMENT_SHADER_ATI = 0x8200