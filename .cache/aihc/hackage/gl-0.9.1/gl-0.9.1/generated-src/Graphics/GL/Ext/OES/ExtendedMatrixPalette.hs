-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.ExtendedMatrixPalette (
  -- * Extension Support
    gl_OES_extended_matrix_palette
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_extended_matrix_palette.txt GL_OES_extended_matrix_palette> extension is available.

gl_OES_extended_matrix_palette :: Bool
gl_OES_extended_matrix_palette = member "GL_OES_extended_matrix_palette" extensions
{-# NOINLINE gl_OES_extended_matrix_palette #-}