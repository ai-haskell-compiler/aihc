-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.FragmentPrecisionHigh (
  -- * Extension Support
    gl_OES_fragment_precision_high
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_fragment_precision_high.txt GL_OES_fragment_precision_high> extension is available.

gl_OES_fragment_precision_high :: Bool
gl_OES_fragment_precision_high = member "GL_OES_fragment_precision_high" extensions
{-# NOINLINE gl_OES_fragment_precision_high #-}