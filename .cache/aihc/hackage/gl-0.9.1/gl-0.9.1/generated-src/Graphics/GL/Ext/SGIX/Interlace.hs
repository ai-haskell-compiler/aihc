-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Interlace (
  -- * Extension Support
    gl_SGIX_interlace

  -- * GL_SGIX_interlace
  , pattern GL_INTERLACE_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/interlace.txt GL_SGIX_interlace> extension is available.

gl_SGIX_interlace :: Bool
gl_SGIX_interlace = member "GL_SGIX_interlace" extensions
{-# NOINLINE gl_SGIX_interlace #-}

pattern GL_INTERLACE_SGIX  :: (Eq a, Num a) => a

pattern GL_INTERLACE_SGIX = 0x8094