-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.InternalformatQuery (
  -- * Extension Support
    gl_ARB_internalformat_query

  -- * GL_ARB_internalformat_query
  , glGetInternalformativ
  , pattern GL_NUM_SAMPLE_COUNTS
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/internalformat_query.txt GL_ARB_internalformat_query> extension is available.

gl_ARB_internalformat_query :: Bool
gl_ARB_internalformat_query = member "GL_ARB_internalformat_query" extensions
{-# NOINLINE gl_ARB_internalformat_query #-}