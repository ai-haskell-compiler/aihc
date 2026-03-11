-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.MESA.PackInvert (
  -- * Extension Support
    gl_MESA_pack_invert

  -- * GL_MESA_pack_invert
  , pattern GL_PACK_INVERT_MESA
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/MESA/pack_invert.txt GL_MESA_pack_invert> extension is available.

gl_MESA_pack_invert :: Bool
gl_MESA_pack_invert = member "GL_MESA_pack_invert" extensions
{-# NOINLINE gl_MESA_pack_invert #-}

pattern GL_PACK_INVERT_MESA  :: (Eq a, Num a) => a

pattern GL_PACK_INVERT_MESA = 0x8758