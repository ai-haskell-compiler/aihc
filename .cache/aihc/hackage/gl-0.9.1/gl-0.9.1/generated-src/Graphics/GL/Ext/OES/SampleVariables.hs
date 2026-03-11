-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.SampleVariables (
  -- * Extension Support
    gl_OES_sample_variables
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_sample_variables.txt GL_OES_sample_variables> extension is available.

gl_OES_sample_variables :: Bool
gl_OES_sample_variables = member "GL_OES_sample_variables" extensions
{-# NOINLINE gl_OES_sample_variables #-}