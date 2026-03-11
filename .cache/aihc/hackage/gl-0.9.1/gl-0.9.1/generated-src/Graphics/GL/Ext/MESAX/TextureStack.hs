-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.MESAX.TextureStack (
  -- * Extension Support
    gl_MESAX_texture_stack

  -- * GL_MESAX_texture_stack
  , pattern GL_PROXY_TEXTURE_1D_STACK_MESAX
  , pattern GL_PROXY_TEXTURE_2D_STACK_MESAX
  , pattern GL_TEXTURE_1D_STACK_BINDING_MESAX
  , pattern GL_TEXTURE_1D_STACK_MESAX
  , pattern GL_TEXTURE_2D_STACK_BINDING_MESAX
  , pattern GL_TEXTURE_2D_STACK_MESAX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/MESAX/texture_stack.txt GL_MESAX_texture_stack> extension is available.

gl_MESAX_texture_stack :: Bool
gl_MESAX_texture_stack = member "GL_MESAX_texture_stack" extensions
{-# NOINLINE gl_MESAX_texture_stack #-}

pattern GL_PROXY_TEXTURE_1D_STACK_MESAX  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_1D_STACK_MESAX = 0x875B

pattern GL_PROXY_TEXTURE_2D_STACK_MESAX  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_2D_STACK_MESAX = 0x875C

pattern GL_TEXTURE_1D_STACK_BINDING_MESAX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_1D_STACK_BINDING_MESAX = 0x875D

pattern GL_TEXTURE_1D_STACK_MESAX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_1D_STACK_MESAX = 0x8759

pattern GL_TEXTURE_2D_STACK_BINDING_MESAX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_2D_STACK_BINDING_MESAX = 0x875E

pattern GL_TEXTURE_2D_STACK_MESAX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_2D_STACK_MESAX = 0x875A