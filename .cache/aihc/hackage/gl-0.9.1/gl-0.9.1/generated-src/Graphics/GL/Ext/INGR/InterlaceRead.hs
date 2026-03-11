-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INGR.InterlaceRead (
  -- * Extension Support
    gl_INGR_interlace_read

  -- * GL_INGR_interlace_read
  , pattern GL_INTERLACE_READ_INGR
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/INGR/interlace_read.txt GL_INGR_interlace_read> extension is available.

gl_INGR_interlace_read :: Bool
gl_INGR_interlace_read = member "GL_INGR_interlace_read" extensions
{-# NOINLINE gl_INGR_interlace_read #-}

pattern GL_INTERLACE_READ_INGR  :: (Eq a, Num a) => a

pattern GL_INTERLACE_READ_INGR = 0x8568