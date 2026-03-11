-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureNorm16 (
  -- * Extension Support
    gl_EXT_texture_norm16

  -- * GL_EXT_texture_norm16
  , pattern GL_R16_EXT
  , pattern GL_R16_SNORM_EXT
  , pattern GL_RG16_EXT
  , pattern GL_RG16_SNORM_EXT
  , pattern GL_RGB16_EXT
  , pattern GL_RGB16_SNORM_EXT
  , pattern GL_RGBA16_EXT
  , pattern GL_RGBA16_SNORM_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the GL_EXT_texture_norm16 extension is available.

gl_EXT_texture_norm16 :: Bool
gl_EXT_texture_norm16 = member "GL_EXT_texture_norm16" extensions
{-# NOINLINE gl_EXT_texture_norm16 #-}

pattern GL_R16_EXT  :: (Eq a, Num a) => a

pattern GL_R16_EXT = 0x822A

pattern GL_RG16_EXT  :: (Eq a, Num a) => a

pattern GL_RG16_EXT = 0x822C

pattern GL_RGB16_SNORM_EXT  :: (Eq a, Num a) => a

pattern GL_RGB16_SNORM_EXT = 0x8F9A