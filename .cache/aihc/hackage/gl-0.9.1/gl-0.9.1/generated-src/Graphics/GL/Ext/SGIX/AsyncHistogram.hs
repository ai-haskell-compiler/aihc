-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.AsyncHistogram (
  -- * Extension Support
    gl_SGIX_async_histogram

  -- * GL_SGIX_async_histogram
  , pattern GL_ASYNC_HISTOGRAM_SGIX
  , pattern GL_MAX_ASYNC_HISTOGRAM_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/async_histogram.txt GL_SGIX_async_histogram> extension is available.

gl_SGIX_async_histogram :: Bool
gl_SGIX_async_histogram = member "GL_SGIX_async_histogram" extensions
{-# NOINLINE gl_SGIX_async_histogram #-}

pattern GL_ASYNC_HISTOGRAM_SGIX  :: (Eq a, Num a) => a

pattern GL_ASYNC_HISTOGRAM_SGIX = 0x832C

pattern GL_MAX_ASYNC_HISTOGRAM_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_ASYNC_HISTOGRAM_SGIX = 0x832D