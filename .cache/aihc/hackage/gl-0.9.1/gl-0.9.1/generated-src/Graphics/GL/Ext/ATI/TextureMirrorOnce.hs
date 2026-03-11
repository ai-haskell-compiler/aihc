-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.TextureMirrorOnce (
  -- * Extension Support
    gl_ATI_texture_mirror_once

  -- * GL_ATI_texture_mirror_once
  , pattern GL_MIRROR_CLAMP_ATI
  , pattern GL_MIRROR_CLAMP_TO_EDGE_ATI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/texture_mirror_once.txt GL_ATI_texture_mirror_once> extension is available.

gl_ATI_texture_mirror_once :: Bool
gl_ATI_texture_mirror_once = member "GL_ATI_texture_mirror_once" extensions
{-# NOINLINE gl_ATI_texture_mirror_once #-}

pattern GL_MIRROR_CLAMP_ATI  :: (Eq a, Num a) => a

pattern GL_MIRROR_CLAMP_ATI = 0x8742

pattern GL_MIRROR_CLAMP_TO_EDGE_ATI  :: (Eq a, Num a) => a

pattern GL_MIRROR_CLAMP_TO_EDGE_ATI = 0x8743