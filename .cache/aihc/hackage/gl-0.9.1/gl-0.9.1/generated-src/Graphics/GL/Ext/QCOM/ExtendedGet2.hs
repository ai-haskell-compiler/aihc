-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.QCOM.ExtendedGet2 (
  -- * Extension Support
    gl_QCOM_extended_get2

  -- * GL_QCOM_extended_get2
  , glExtGetProgramBinarySourceQCOM
  , glExtGetProgramsQCOM
  , glExtGetShadersQCOM
  , glExtIsProgramBinaryQCOM
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/QCOM/QCOM_extended_get2.txt GL_QCOM_extended_get2> extension is available.

gl_QCOM_extended_get2 :: Bool
gl_QCOM_extended_get2 = member "GL_QCOM_extended_get2" extensions
{-# NOINLINE gl_QCOM_extended_get2 #-}

-- | Usage: @'glExtGetProgramBinarySourceQCOM' program shadertype source length@


glExtGetProgramBinarySourceQCOM :: MonadIO m => GLuint -> GLenum -> Ptr GLchar -> Ptr GLint -> m ()
glExtGetProgramBinarySourceQCOM = ffiuintenumPtrcharPtrintIOV glExtGetProgramBinarySourceQCOMFunPtr

glExtGetProgramBinarySourceQCOMFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> Ptr GLint -> IO ())
glExtGetProgramBinarySourceQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetProgramBinarySourceQCOM")

{-# NOINLINE glExtGetProgramBinarySourceQCOMFunPtr #-}

-- | Usage: @'glExtGetProgramsQCOM' programs maxPrograms numPrograms@
--
-- The length of @programs@ should be @maxPrograms@.
--
-- The length of @numPrograms@ should be @1@.


glExtGetProgramsQCOM :: MonadIO m => Ptr GLuint -> GLint -> Ptr GLint -> m ()
glExtGetProgramsQCOM = ffiPtruintintPtrintIOV glExtGetProgramsQCOMFunPtr

glExtGetProgramsQCOMFunPtr :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
glExtGetProgramsQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetProgramsQCOM")

{-# NOINLINE glExtGetProgramsQCOMFunPtr #-}

-- | Usage: @'glExtGetShadersQCOM' shaders maxShaders numShaders@
--
-- The length of @shaders@ should be @maxShaders@.
--
-- The length of @numShaders@ should be @1@.


glExtGetShadersQCOM :: MonadIO m => Ptr GLuint -> GLint -> Ptr GLint -> m ()
glExtGetShadersQCOM = ffiPtruintintPtrintIOV glExtGetShadersQCOMFunPtr

glExtGetShadersQCOMFunPtr :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
glExtGetShadersQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetShadersQCOM")

{-# NOINLINE glExtGetShadersQCOMFunPtr #-}

-- | Usage: @'glExtIsProgramBinaryQCOM' program@


glExtIsProgramBinaryQCOM :: MonadIO m => GLuint -> m GLboolean
glExtIsProgramBinaryQCOM = ffiuintIOboolean glExtIsProgramBinaryQCOMFunPtr

glExtIsProgramBinaryQCOMFunPtr :: FunPtr (GLuint -> IO GLboolean)
glExtIsProgramBinaryQCOMFunPtr = unsafePerformIO (getProcAddress "glExtIsProgramBinaryQCOM")

{-# NOINLINE glExtIsProgramBinaryQCOMFunPtr #-}