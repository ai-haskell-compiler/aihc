-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.InternalformatSampleQuery (
  -- * Extension Support
    gl_NV_internalformat_sample_query

  -- * GL_NV_internalformat_sample_query
  , glGetInternalformatSampleivNV
  , pattern GL_CONFORMANT_NV
  , pattern GL_MULTISAMPLES_NV
  , pattern GL_RENDERBUFFER
  , pattern GL_SUPERSAMPLE_SCALE_X_NV
  , pattern GL_SUPERSAMPLE_SCALE_Y_NV
  , pattern GL_TEXTURE_2D_MULTISAMPLE
  , pattern GL_TEXTURE_2D_MULTISAMPLE_ARRAY
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_internalformat_sample_query.txt GL_NV_internalformat_sample_query> extension is available.

gl_NV_internalformat_sample_query :: Bool
gl_NV_internalformat_sample_query = member "GL_NV_internalformat_sample_query" extensions
{-# NOINLINE gl_NV_internalformat_sample_query #-}

-- | Usage: @'glGetInternalformatSampleivNV' target internalformat samples pname bufSize params@
--
-- The length of @params@ should be @bufSize@.


glGetInternalformatSampleivNV :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLenum -> GLsizei -> Ptr GLint -> m ()
glGetInternalformatSampleivNV = ffienumenumsizeienumsizeiPtrintIOV glGetInternalformatSampleivNVFunPtr

glGetInternalformatSampleivNVFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLsizei -> Ptr GLint -> IO ())
glGetInternalformatSampleivNVFunPtr = unsafePerformIO (getProcAddress "glGetInternalformatSampleivNV")

{-# NOINLINE glGetInternalformatSampleivNVFunPtr #-}

pattern GL_CONFORMANT_NV  :: (Eq a, Num a) => a

pattern GL_CONFORMANT_NV = 0x9374

pattern GL_MULTISAMPLES_NV  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLES_NV = 0x9371

pattern GL_SUPERSAMPLE_SCALE_X_NV  :: (Eq a, Num a) => a

pattern GL_SUPERSAMPLE_SCALE_X_NV = 0x9372

pattern GL_SUPERSAMPLE_SCALE_Y_NV  :: (Eq a, Num a) => a

pattern GL_SUPERSAMPLE_SCALE_Y_NV = 0x9373