-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureMirroredRepeat (
  -- * Extension Support
    gl_OES_texture_mirrored_repeat

  -- * GL_OES_texture_mirrored_repeat
  , pattern GL_MIRRORED_REPEAT_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_mirrored_repeat.txt GL_OES_texture_mirrored_repeat> extension is available.

gl_OES_texture_mirrored_repeat :: Bool
gl_OES_texture_mirrored_repeat = member "GL_OES_texture_mirrored_repeat" extensions
{-# NOINLINE gl_OES_texture_mirrored_repeat #-}

pattern GL_MIRRORED_REPEAT_OES  :: (Eq a, Num a) => a

pattern GL_MIRRORED_REPEAT_OES = 0x8370