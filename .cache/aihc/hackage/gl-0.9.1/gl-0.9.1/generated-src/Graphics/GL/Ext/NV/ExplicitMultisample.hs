-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ExplicitMultisample (
  -- * Extension Support
    gl_NV_explicit_multisample

  -- * GL_NV_explicit_multisample
  , glGetMultisamplefvNV
  , glSampleMaskIndexedNV
  , glTexRenderbufferNV
  , pattern GL_INT_SAMPLER_RENDERBUFFER_NV
  , pattern GL_MAX_SAMPLE_MASK_WORDS_NV
  , pattern GL_SAMPLER_RENDERBUFFER_NV
  , pattern GL_SAMPLE_MASK_NV
  , pattern GL_SAMPLE_MASK_VALUE_NV
  , pattern GL_SAMPLE_POSITION_NV
  , pattern GL_TEXTURE_BINDING_RENDERBUFFER_NV
  , pattern GL_TEXTURE_RENDERBUFFER_DATA_STORE_BINDING_NV
  , pattern GL_TEXTURE_RENDERBUFFER_NV
  , pattern GL_UNSIGNED_INT_SAMPLER_RENDERBUFFER_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/explicit_multisample.txt GL_NV_explicit_multisample> extension is available.

gl_NV_explicit_multisample :: Bool
gl_NV_explicit_multisample = member "GL_NV_explicit_multisample" extensions
{-# NOINLINE gl_NV_explicit_multisample #-}

-- | Usage: @'glGetMultisamplefvNV' pname index val@
--
-- The parameter @pname@ is a @GetMultisamplePNameNV@.
--
-- The length of @val@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetMultisamplefv'.


glGetMultisamplefvNV :: MonadIO m => GLenum -> GLuint -> Ptr GLfloat -> m ()
glGetMultisamplefvNV = ffienumuintPtrfloatIOV glGetMultisamplefvNVFunPtr

glGetMultisamplefvNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
glGetMultisamplefvNVFunPtr = unsafePerformIO (getProcAddress "glGetMultisamplefvNV")

{-# NOINLINE glGetMultisamplefvNVFunPtr #-}

-- | Usage: @'glSampleMaskIndexedNV' index mask@
--
-- The parameter @mask@ is a @SampleMaskNV@.


glSampleMaskIndexedNV :: MonadIO m => GLuint -> GLbitfield -> m ()
glSampleMaskIndexedNV = ffiuintbitfieldIOV glSampleMaskIndexedNVFunPtr

glSampleMaskIndexedNVFunPtr :: FunPtr (GLuint -> GLbitfield -> IO ())
glSampleMaskIndexedNVFunPtr = unsafePerformIO (getProcAddress "glSampleMaskIndexedNV")

{-# NOINLINE glSampleMaskIndexedNVFunPtr #-}

-- | Usage: @'glTexRenderbufferNV' target renderbuffer@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.


glTexRenderbufferNV :: MonadIO m => GLenum -> GLuint -> m ()
glTexRenderbufferNV = ffienumuintIOV glTexRenderbufferNVFunPtr

glTexRenderbufferNVFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glTexRenderbufferNVFunPtr = unsafePerformIO (getProcAddress "glTexRenderbufferNV")

{-# NOINLINE glTexRenderbufferNVFunPtr #-}

pattern GL_INT_SAMPLER_RENDERBUFFER_NV  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_RENDERBUFFER_NV = 0x8E57

pattern GL_MAX_SAMPLE_MASK_WORDS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_SAMPLE_MASK_WORDS_NV = 0x8E59

pattern GL_SAMPLER_RENDERBUFFER_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLER_RENDERBUFFER_NV = 0x8E56

pattern GL_SAMPLE_MASK_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLE_MASK_NV = 0x8E51

pattern GL_SAMPLE_MASK_VALUE_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLE_MASK_VALUE_NV = 0x8E52

pattern GL_SAMPLE_POSITION_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLE_POSITION_NV = 0x8E50

pattern GL_TEXTURE_BINDING_RENDERBUFFER_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_RENDERBUFFER_NV = 0x8E53

pattern GL_TEXTURE_RENDERBUFFER_DATA_STORE_BINDING_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RENDERBUFFER_DATA_STORE_BINDING_NV = 0x8E54

pattern GL_TEXTURE_RENDERBUFFER_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RENDERBUFFER_NV = 0x8E55

pattern GL_UNSIGNED_INT_SAMPLER_RENDERBUFFER_NV  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_RENDERBUFFER_NV = 0x8E58