-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TransposeMatrix (
  -- * Extension Support
    gl_ARB_transpose_matrix

  -- * GL_ARB_transpose_matrix
  , glLoadTransposeMatrixdARB
  , glLoadTransposeMatrixfARB
  , glMultTransposeMatrixdARB
  , glMultTransposeMatrixfARB
  , pattern GL_TRANSPOSE_COLOR_MATRIX_ARB
  , pattern GL_TRANSPOSE_MODELVIEW_MATRIX_ARB
  , pattern GL_TRANSPOSE_PROJECTION_MATRIX_ARB
  , pattern GL_TRANSPOSE_TEXTURE_MATRIX_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/transpose_matrix.txt GL_ARB_transpose_matrix> extension is available.

gl_ARB_transpose_matrix :: Bool
gl_ARB_transpose_matrix = member "GL_ARB_transpose_matrix" extensions
{-# NOINLINE gl_ARB_transpose_matrix #-}

-- | Usage: @'glLoadTransposeMatrixdARB' m@
--
-- The length of @m@ should be @16@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glLoadTransposeMatrixd'.


glLoadTransposeMatrixdARB :: MonadIO m => Ptr GLdouble -> m ()
glLoadTransposeMatrixdARB = ffiPtrdoubleIOV glLoadTransposeMatrixdARBFunPtr

glLoadTransposeMatrixdARBFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glLoadTransposeMatrixdARBFunPtr = unsafePerformIO (getProcAddress "glLoadTransposeMatrixdARB")

{-# NOINLINE glLoadTransposeMatrixdARBFunPtr #-}

-- | Usage: @'glLoadTransposeMatrixfARB' m@
--
-- The length of @m@ should be @16@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glLoadTransposeMatrixf'.


glLoadTransposeMatrixfARB :: MonadIO m => Ptr GLfloat -> m ()
glLoadTransposeMatrixfARB = ffiPtrfloatIOV glLoadTransposeMatrixfARBFunPtr

glLoadTransposeMatrixfARBFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glLoadTransposeMatrixfARBFunPtr = unsafePerformIO (getProcAddress "glLoadTransposeMatrixfARB")

{-# NOINLINE glLoadTransposeMatrixfARBFunPtr #-}

-- | Usage: @'glMultTransposeMatrixdARB' m@
--
-- The length of @m@ should be @16@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultTransposeMatrixd'.


glMultTransposeMatrixdARB :: MonadIO m => Ptr GLdouble -> m ()
glMultTransposeMatrixdARB = ffiPtrdoubleIOV glMultTransposeMatrixdARBFunPtr

glMultTransposeMatrixdARBFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glMultTransposeMatrixdARBFunPtr = unsafePerformIO (getProcAddress "glMultTransposeMatrixdARB")

{-# NOINLINE glMultTransposeMatrixdARBFunPtr #-}

-- | Usage: @'glMultTransposeMatrixfARB' m@
--
-- The length of @m@ should be @16@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultTransposeMatrixf'.


glMultTransposeMatrixfARB :: MonadIO m => Ptr GLfloat -> m ()
glMultTransposeMatrixfARB = ffiPtrfloatIOV glMultTransposeMatrixfARBFunPtr

glMultTransposeMatrixfARBFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glMultTransposeMatrixfARBFunPtr = unsafePerformIO (getProcAddress "glMultTransposeMatrixfARB")

{-# NOINLINE glMultTransposeMatrixfARBFunPtr #-}

pattern GL_TRANSPOSE_COLOR_MATRIX_ARB  :: (Eq a, Num a) => a

pattern GL_TRANSPOSE_COLOR_MATRIX_ARB = 0x84E6

pattern GL_TRANSPOSE_MODELVIEW_MATRIX_ARB  :: (Eq a, Num a) => a

pattern GL_TRANSPOSE_MODELVIEW_MATRIX_ARB = 0x84E3

pattern GL_TRANSPOSE_PROJECTION_MATRIX_ARB  :: (Eq a, Num a) => a

pattern GL_TRANSPOSE_PROJECTION_MATRIX_ARB = 0x84E4

pattern GL_TRANSPOSE_TEXTURE_MATRIX_ARB  :: (Eq a, Num a) => a

pattern GL_TRANSPOSE_TEXTURE_MATRIX_ARB = 0x84E5