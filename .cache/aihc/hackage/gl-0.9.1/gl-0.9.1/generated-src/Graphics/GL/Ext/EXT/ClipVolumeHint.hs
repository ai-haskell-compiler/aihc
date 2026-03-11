-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ClipVolumeHint (
  -- * Extension Support
    gl_EXT_clip_volume_hint

  -- * GL_EXT_clip_volume_hint
  , pattern GL_CLIP_VOLUME_CLIPPING_HINT_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/clip_volume_hint.txt GL_EXT_clip_volume_hint> extension is available.

gl_EXT_clip_volume_hint :: Bool
gl_EXT_clip_volume_hint = member "GL_EXT_clip_volume_hint" extensions
{-# NOINLINE gl_EXT_clip_volume_hint #-}

pattern GL_CLIP_VOLUME_CLIPPING_HINT_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_VOLUME_CLIPPING_HINT_EXT = 0x80F0