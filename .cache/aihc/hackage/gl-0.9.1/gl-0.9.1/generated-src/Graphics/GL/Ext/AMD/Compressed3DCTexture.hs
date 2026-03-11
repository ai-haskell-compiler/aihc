-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.Compressed3DCTexture (
  -- * Extension Support
    gl_AMD_compressed_3DC_texture

  -- * GL_AMD_compressed_3DC_texture
  , pattern GL_3DC_XY_AMD
  , pattern GL_3DC_X_AMD
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/AMD/AMD_compressed_3DC_texture.txt GL_AMD_compressed_3DC_texture> extension is available.

gl_AMD_compressed_3DC_texture :: Bool
gl_AMD_compressed_3DC_texture = member "GL_AMD_compressed_3DC_texture" extensions
{-# NOINLINE gl_AMD_compressed_3DC_texture #-}

pattern GL_3DC_XY_AMD  :: (Eq a, Num a) => a

pattern GL_3DC_XY_AMD = 0x87FA

pattern GL_3DC_X_AMD  :: (Eq a, Num a) => a

pattern GL_3DC_X_AMD = 0x87F9