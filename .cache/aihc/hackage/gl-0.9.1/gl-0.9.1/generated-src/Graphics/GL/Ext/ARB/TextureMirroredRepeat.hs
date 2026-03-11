-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureMirroredRepeat (
  -- * Extension Support
    gl_ARB_texture_mirrored_repeat

  -- * GL_ARB_texture_mirrored_repeat
  , pattern GL_MIRRORED_REPEAT_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_mirrored_repeat.txt GL_ARB_texture_mirrored_repeat> extension is available.

gl_ARB_texture_mirrored_repeat :: Bool
gl_ARB_texture_mirrored_repeat = member "GL_ARB_texture_mirrored_repeat" extensions
{-# NOINLINE gl_ARB_texture_mirrored_repeat #-}

pattern GL_MIRRORED_REPEAT_ARB  :: (Eq a, Num a) => a

pattern GL_MIRRORED_REPEAT_ARB = 0x8370