-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.PrimitiveRestart (
  -- * Extension Support
    gl_NV_primitive_restart

  -- * GL_NV_primitive_restart
  , glPrimitiveRestartIndexNV
  , glPrimitiveRestartNV
  , pattern GL_PRIMITIVE_RESTART_INDEX_NV
  , pattern GL_PRIMITIVE_RESTART_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/primitive_restart.txt GL_NV_primitive_restart> extension is available.

gl_NV_primitive_restart :: Bool
gl_NV_primitive_restart = member "GL_NV_primitive_restart" extensions
{-# NOINLINE gl_NV_primitive_restart #-}

-- | Usage: @'glPrimitiveRestartIndexNV' index@


glPrimitiveRestartIndexNV :: MonadIO m => GLuint -> m ()
glPrimitiveRestartIndexNV = ffiuintIOV glPrimitiveRestartIndexNVFunPtr

glPrimitiveRestartIndexNVFunPtr :: FunPtr (GLuint -> IO ())
glPrimitiveRestartIndexNVFunPtr = unsafePerformIO (getProcAddress "glPrimitiveRestartIndexNV")

{-# NOINLINE glPrimitiveRestartIndexNVFunPtr #-}

-- | Usage: @'glPrimitiveRestartNV'@


glPrimitiveRestartNV :: MonadIO m => m ()
glPrimitiveRestartNV = ffiIOV glPrimitiveRestartNVFunPtr

glPrimitiveRestartNVFunPtr :: FunPtr (IO ())
glPrimitiveRestartNVFunPtr = unsafePerformIO (getProcAddress "glPrimitiveRestartNV")

{-# NOINLINE glPrimitiveRestartNVFunPtr #-}

pattern GL_PRIMITIVE_RESTART_INDEX_NV  :: (Eq a, Num a) => a

pattern GL_PRIMITIVE_RESTART_INDEX_NV = 0x8559

pattern GL_PRIMITIVE_RESTART_NV  :: (Eq a, Num a) => a

pattern GL_PRIMITIVE_RESTART_NV = 0x8558