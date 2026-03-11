-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.SamplePositions (
  -- * Extension Support
    gl_AMD_sample_positions

  -- * GL_AMD_sample_positions
  , glSetMultisamplefvAMD
  , pattern GL_SUBSAMPLE_DISTANCE_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/sample_positions.txt GL_AMD_sample_positions> extension is available.

gl_AMD_sample_positions :: Bool
gl_AMD_sample_positions = member "GL_AMD_sample_positions" extensions
{-# NOINLINE gl_AMD_sample_positions #-}

-- | Usage: @'glSetMultisamplefvAMD' pname index val@
--
-- The length of @val@ should be @2@.


glSetMultisamplefvAMD :: MonadIO m => GLenum -> GLuint -> Ptr GLfloat -> m ()
glSetMultisamplefvAMD = ffienumuintPtrfloatIOV glSetMultisamplefvAMDFunPtr

glSetMultisamplefvAMDFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
glSetMultisamplefvAMDFunPtr = unsafePerformIO (getProcAddress "glSetMultisamplefvAMD")

{-# NOINLINE glSetMultisamplefvAMDFunPtr #-}