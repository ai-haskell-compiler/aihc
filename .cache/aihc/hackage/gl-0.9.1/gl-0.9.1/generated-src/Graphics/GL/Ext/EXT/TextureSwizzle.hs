-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureSwizzle (
  -- * Extension Support
    gl_EXT_texture_swizzle

  -- * GL_EXT_texture_swizzle
  , pattern GL_TEXTURE_SWIZZLE_A_EXT
  , pattern GL_TEXTURE_SWIZZLE_B_EXT
  , pattern GL_TEXTURE_SWIZZLE_G_EXT
  , pattern GL_TEXTURE_SWIZZLE_RGBA_EXT
  , pattern GL_TEXTURE_SWIZZLE_R_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_swizzle.txt GL_EXT_texture_swizzle> extension is available.

gl_EXT_texture_swizzle :: Bool
gl_EXT_texture_swizzle = member "GL_EXT_texture_swizzle" extensions
{-# NOINLINE gl_EXT_texture_swizzle #-}

pattern GL_TEXTURE_SWIZZLE_A_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SWIZZLE_A_EXT = 0x8E45

pattern GL_TEXTURE_SWIZZLE_B_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SWIZZLE_B_EXT = 0x8E44

pattern GL_TEXTURE_SWIZZLE_G_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SWIZZLE_G_EXT = 0x8E43

pattern GL_TEXTURE_SWIZZLE_RGBA_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SWIZZLE_RGBA_EXT = 0x8E46

pattern GL_TEXTURE_SWIZZLE_R_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SWIZZLE_R_EXT = 0x8E42