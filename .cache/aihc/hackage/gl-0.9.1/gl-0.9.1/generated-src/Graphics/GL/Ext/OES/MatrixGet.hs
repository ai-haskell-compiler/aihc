-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.MatrixGet (
  -- * Extension Support
    gl_OES_matrix_get

  -- * GL_OES_matrix_get
  , pattern GL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES
  , pattern GL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES
  , pattern GL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_matrix_get.txt GL_OES_matrix_get> extension is available.

gl_OES_matrix_get :: Bool
gl_OES_matrix_get = member "GL_OES_matrix_get" extensions
{-# NOINLINE gl_OES_matrix_get #-}

pattern GL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES  :: (Eq a, Num a) => a

pattern GL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES = 0x898D

pattern GL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES  :: (Eq a, Num a) => a

pattern GL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES = 0x898E

pattern GL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES = 0x898F