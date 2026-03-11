-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.GenerateMipmap (
  -- * Extension Support
    gl_SGIS_generate_mipmap

  -- * GL_SGIS_generate_mipmap
  , pattern GL_GENERATE_MIPMAP_HINT_SGIS
  , pattern GL_GENERATE_MIPMAP_SGIS
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/generate_mipmap.txt GL_SGIS_generate_mipmap> extension is available.

gl_SGIS_generate_mipmap :: Bool
gl_SGIS_generate_mipmap = member "GL_SGIS_generate_mipmap" extensions
{-# NOINLINE gl_SGIS_generate_mipmap #-}

pattern GL_GENERATE_MIPMAP_HINT_SGIS  :: (Eq a, Num a) => a

pattern GL_GENERATE_MIPMAP_HINT_SGIS = 0x8192

pattern GL_GENERATE_MIPMAP_SGIS  :: (Eq a, Num a) => a

pattern GL_GENERATE_MIPMAP_SGIS = 0x8191