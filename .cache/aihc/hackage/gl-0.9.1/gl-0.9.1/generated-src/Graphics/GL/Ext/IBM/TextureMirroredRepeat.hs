-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IBM.TextureMirroredRepeat (
  -- * Extension Support
    gl_IBM_texture_mirrored_repeat

  -- * GL_IBM_texture_mirrored_repeat
  , pattern GL_MIRRORED_REPEAT_IBM
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/IBM/texture_mirrored_repeat.txt GL_IBM_texture_mirrored_repeat> extension is available.

gl_IBM_texture_mirrored_repeat :: Bool
gl_IBM_texture_mirrored_repeat = member "GL_IBM_texture_mirrored_repeat" extensions
{-# NOINLINE gl_IBM_texture_mirrored_repeat #-}

pattern GL_MIRRORED_REPEAT_IBM  :: (Eq a, Num a) => a

pattern GL_MIRRORED_REPEAT_IBM = 0x8370