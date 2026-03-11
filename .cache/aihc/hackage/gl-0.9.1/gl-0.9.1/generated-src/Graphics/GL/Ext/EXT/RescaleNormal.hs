-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.RescaleNormal (
  -- * Extension Support
    gl_EXT_rescale_normal

  -- * GL_EXT_rescale_normal
  , pattern GL_RESCALE_NORMAL_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/rescale_normal.txt GL_EXT_rescale_normal> extension is available.

gl_EXT_rescale_normal :: Bool
gl_EXT_rescale_normal = member "GL_EXT_rescale_normal" extensions
{-# NOINLINE gl_EXT_rescale_normal #-}

pattern GL_RESCALE_NORMAL_EXT  :: (Eq a, Num a) => a

pattern GL_RESCALE_NORMAL_EXT = 0x803A