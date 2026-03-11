-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureStorageMultisample2dArray (
  -- * Extension Support
    gl_OES_texture_storage_multisample_2d_array

  -- * GL_OES_texture_storage_multisample_2d_array
  , glTexStorage3DMultisampleOES
  , pattern GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY_OES
  , pattern GL_SAMPLER_2D_MULTISAMPLE_ARRAY_OES
  , pattern GL_TEXTURE_2D_MULTISAMPLE_ARRAY_OES
  , pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY_OES
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_storage_multisample_2d_array.txt GL_OES_texture_storage_multisample_2d_array> extension is available.

gl_OES_texture_storage_multisample_2d_array :: Bool
gl_OES_texture_storage_multisample_2d_array = member "GL_OES_texture_storage_multisample_2d_array" extensions
{-# NOINLINE gl_OES_texture_storage_multisample_2d_array #-}

-- | Usage: @'glTexStorage3DMultisampleOES' target samples internalformat width height depth fixedsamplelocations@
--
-- The parameter @fixedsamplelocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexStorage3DMultisample'.


glTexStorage3DMultisampleOES :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
glTexStorage3DMultisampleOES = ffienumsizeienumsizeisizeisizeibooleanIOV glTexStorage3DMultisampleOESFunPtr

glTexStorage3DMultisampleOESFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTexStorage3DMultisampleOESFunPtr = unsafePerformIO (getProcAddress "glTexStorage3DMultisampleOES")

{-# NOINLINE glTexStorage3DMultisampleOESFunPtr #-}

pattern GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY_OES = 0x910C

pattern GL_SAMPLER_2D_MULTISAMPLE_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_MULTISAMPLE_ARRAY_OES = 0x910B

pattern GL_TEXTURE_2D_MULTISAMPLE_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_2D_MULTISAMPLE_ARRAY_OES = 0x9102

pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY_OES = 0x9105

pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY_OES = 0x910D