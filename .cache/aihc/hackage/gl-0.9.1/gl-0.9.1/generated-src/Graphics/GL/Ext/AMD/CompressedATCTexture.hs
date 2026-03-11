-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.CompressedATCTexture (
  -- * Extension Support
    gl_AMD_compressed_ATC_texture

  -- * GL_AMD_compressed_ATC_texture
  , pattern GL_ATC_RGBA_EXPLICIT_ALPHA_AMD
  , pattern GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD
  , pattern GL_ATC_RGB_AMD
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/AMD/AMD_compressed_ATC_texture.txt GL_AMD_compressed_ATC_texture> extension is available.

gl_AMD_compressed_ATC_texture :: Bool
gl_AMD_compressed_ATC_texture = member "GL_AMD_compressed_ATC_texture" extensions
{-# NOINLINE gl_AMD_compressed_ATC_texture #-}

pattern GL_ATC_RGBA_EXPLICIT_ALPHA_AMD  :: (Eq a, Num a) => a

pattern GL_ATC_RGBA_EXPLICIT_ALPHA_AMD = 0x8C93

pattern GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD  :: (Eq a, Num a) => a

pattern GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD = 0x87EE

pattern GL_ATC_RGB_AMD  :: (Eq a, Num a) => a

pattern GL_ATC_RGB_AMD = 0x8C92