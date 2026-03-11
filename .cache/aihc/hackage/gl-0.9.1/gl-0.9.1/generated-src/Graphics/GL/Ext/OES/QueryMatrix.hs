-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.QueryMatrix (
  -- * Extension Support
    gl_OES_query_matrix

  -- * GL_OES_query_matrix
  , glQueryMatrixxOES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_query_matrix.txt GL_OES_query_matrix> extension is available.

gl_OES_query_matrix :: Bool
gl_OES_query_matrix = member "GL_OES_query_matrix" extensions
{-# NOINLINE gl_OES_query_matrix #-}

-- | Usage: @'glQueryMatrixxOES' mantissa exponent@
--
-- The length of @mantissa@ should be @16@.
--
-- The length of @exponent@ should be @16@.


glQueryMatrixxOES :: MonadIO m => Ptr GLfixed -> Ptr GLint -> m GLbitfield
glQueryMatrixxOES = ffiPtrfixedPtrintIObitfield glQueryMatrixxOESFunPtr

glQueryMatrixxOESFunPtr :: FunPtr (Ptr GLfixed -> Ptr GLint -> IO GLbitfield)
glQueryMatrixxOESFunPtr = unsafePerformIO (getProcAddress "glQueryMatrixxOES")

{-# NOINLINE glQueryMatrixxOESFunPtr #-}