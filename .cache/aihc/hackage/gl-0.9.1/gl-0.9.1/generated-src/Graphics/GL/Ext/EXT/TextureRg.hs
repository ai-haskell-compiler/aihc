-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureRg (
  -- * Extension Support
    gl_EXT_texture_rg

  -- * GL_EXT_texture_rg
  , pattern GL_R8_EXT
  , pattern GL_RED_EXT
  , pattern GL_RG8_EXT
  , pattern GL_RG_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_rg.txt GL_EXT_texture_rg> extension is available.

gl_EXT_texture_rg :: Bool
gl_EXT_texture_rg = member "GL_EXT_texture_rg" extensions
{-# NOINLINE gl_EXT_texture_rg #-}

pattern GL_RED_EXT  :: (Eq a, Num a) => a

pattern GL_RED_EXT = 0x1903

pattern GL_RG_EXT  :: (Eq a, Num a) => a

pattern GL_RG_EXT = 0x8227