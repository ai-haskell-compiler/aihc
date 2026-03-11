-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureMirrorClamp (
  -- * Extension Support
    gl_EXT_texture_mirror_clamp

  -- * GL_EXT_texture_mirror_clamp
  , pattern GL_MIRROR_CLAMP_EXT
  , pattern GL_MIRROR_CLAMP_TO_BORDER_EXT
  , pattern GL_MIRROR_CLAMP_TO_EDGE_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_mirror_clamp.txt GL_EXT_texture_mirror_clamp> extension is available.

gl_EXT_texture_mirror_clamp :: Bool
gl_EXT_texture_mirror_clamp = member "GL_EXT_texture_mirror_clamp" extensions
{-# NOINLINE gl_EXT_texture_mirror_clamp #-}

pattern GL_MIRROR_CLAMP_EXT  :: (Eq a, Num a) => a

pattern GL_MIRROR_CLAMP_EXT = 0x8742

pattern GL_MIRROR_CLAMP_TO_BORDER_EXT  :: (Eq a, Num a) => a

pattern GL_MIRROR_CLAMP_TO_BORDER_EXT = 0x8912

pattern GL_MIRROR_CLAMP_TO_EDGE_EXT  :: (Eq a, Num a) => a

pattern GL_MIRROR_CLAMP_TO_EDGE_EXT = 0x8743