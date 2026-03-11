-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.NonSquareMatrices (
  -- * Extension Support
    gl_NV_non_square_matrices

  -- * GL_NV_non_square_matrices
  , glUniformMatrix2x3fvNV
  , glUniformMatrix2x4fvNV
  , glUniformMatrix3x2fvNV
  , glUniformMatrix3x4fvNV
  , glUniformMatrix4x2fvNV
  , glUniformMatrix4x3fvNV
  , pattern GL_FLOAT_MAT2x3_NV
  , pattern GL_FLOAT_MAT2x4_NV
  , pattern GL_FLOAT_MAT3x2_NV
  , pattern GL_FLOAT_MAT3x4_NV
  , pattern GL_FLOAT_MAT4x2_NV
  , pattern GL_FLOAT_MAT4x3_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_non_square_matrices.txt GL_NV_non_square_matrices> extension is available.

gl_NV_non_square_matrices :: Bool
gl_NV_non_square_matrices = member "GL_NV_non_square_matrices" extensions
{-# NOINLINE gl_NV_non_square_matrices #-}

-- | Usage: @'glUniformMatrix2x3fvNV' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*6@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix2x3fv'.


glUniformMatrix2x3fvNV :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix2x3fvNV = ffiintsizeibooleanPtrfloatIOV glUniformMatrix2x3fvNVFunPtr

glUniformMatrix2x3fvNVFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix2x3fvNVFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix2x3fvNV")

{-# NOINLINE glUniformMatrix2x3fvNVFunPtr #-}

-- | Usage: @'glUniformMatrix2x4fvNV' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*8@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix2x4fv'.


glUniformMatrix2x4fvNV :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix2x4fvNV = ffiintsizeibooleanPtrfloatIOV glUniformMatrix2x4fvNVFunPtr

glUniformMatrix2x4fvNVFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix2x4fvNVFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix2x4fvNV")

{-# NOINLINE glUniformMatrix2x4fvNVFunPtr #-}

-- | Usage: @'glUniformMatrix3x2fvNV' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*6@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix3x2fv'.


glUniformMatrix3x2fvNV :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix3x2fvNV = ffiintsizeibooleanPtrfloatIOV glUniformMatrix3x2fvNVFunPtr

glUniformMatrix3x2fvNVFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix3x2fvNVFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix3x2fvNV")

{-# NOINLINE glUniformMatrix3x2fvNVFunPtr #-}

-- | Usage: @'glUniformMatrix3x4fvNV' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*12@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix3x4fv'.


glUniformMatrix3x4fvNV :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix3x4fvNV = ffiintsizeibooleanPtrfloatIOV glUniformMatrix3x4fvNVFunPtr

glUniformMatrix3x4fvNVFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix3x4fvNVFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix3x4fvNV")

{-# NOINLINE glUniformMatrix3x4fvNVFunPtr #-}

-- | Usage: @'glUniformMatrix4x2fvNV' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*8@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix4x2fv'.


glUniformMatrix4x2fvNV :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix4x2fvNV = ffiintsizeibooleanPtrfloatIOV glUniformMatrix4x2fvNVFunPtr

glUniformMatrix4x2fvNVFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix4x2fvNVFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix4x2fvNV")

{-# NOINLINE glUniformMatrix4x2fvNVFunPtr #-}

-- | Usage: @'glUniformMatrix4x3fvNV' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*12@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix4x3fv'.


glUniformMatrix4x3fvNV :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix4x3fvNV = ffiintsizeibooleanPtrfloatIOV glUniformMatrix4x3fvNVFunPtr

glUniformMatrix4x3fvNVFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix4x3fvNVFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix4x3fvNV")

{-# NOINLINE glUniformMatrix4x3fvNVFunPtr #-}

pattern GL_FLOAT_MAT2x3_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_MAT2x3_NV = 0x8B65

pattern GL_FLOAT_MAT2x4_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_MAT2x4_NV = 0x8B66

pattern GL_FLOAT_MAT3x2_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_MAT3x2_NV = 0x8B67

pattern GL_FLOAT_MAT3x4_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_MAT3x4_NV = 0x8B68

pattern GL_FLOAT_MAT4x2_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_MAT4x2_NV = 0x8B69

pattern GL_FLOAT_MAT4x3_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_MAT4x3_NV = 0x8B6A