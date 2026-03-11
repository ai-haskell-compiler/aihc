-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureArray (
  -- * Extension Support
    gl_EXT_texture_array

  -- * GL_EXT_texture_array
  , glFramebufferTextureLayerEXT
  , pattern GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT
  , pattern GL_MAX_ARRAY_TEXTURE_LAYERS_EXT
  , pattern GL_PROXY_TEXTURE_1D_ARRAY_EXT
  , pattern GL_PROXY_TEXTURE_2D_ARRAY_EXT
  , pattern GL_TEXTURE_1D_ARRAY_EXT
  , pattern GL_TEXTURE_2D_ARRAY_EXT
  , pattern GL_TEXTURE_BINDING_1D_ARRAY_EXT
  , pattern GL_TEXTURE_BINDING_2D_ARRAY_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_array.txt GL_EXT_texture_array> extension is available.

gl_EXT_texture_array :: Bool
gl_EXT_texture_array = member "GL_EXT_texture_array" extensions
{-# NOINLINE gl_EXT_texture_array #-}

pattern GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT  :: (Eq a, Num a) => a

pattern GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT = 0x884E

pattern GL_MAX_ARRAY_TEXTURE_LAYERS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_ARRAY_TEXTURE_LAYERS_EXT = 0x88FF

pattern GL_PROXY_TEXTURE_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_1D_ARRAY_EXT = 0x8C19

pattern GL_PROXY_TEXTURE_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_2D_ARRAY_EXT = 0x8C1B

pattern GL_TEXTURE_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_1D_ARRAY_EXT = 0x8C18

pattern GL_TEXTURE_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_2D_ARRAY_EXT = 0x8C1A

pattern GL_TEXTURE_BINDING_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_1D_ARRAY_EXT = 0x8C1C

pattern GL_TEXTURE_BINDING_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_2D_ARRAY_EXT = 0x8C1D