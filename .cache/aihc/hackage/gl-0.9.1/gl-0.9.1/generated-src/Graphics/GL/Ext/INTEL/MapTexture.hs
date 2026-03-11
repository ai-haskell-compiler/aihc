-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INTEL.MapTexture (
  -- * Extension Support
    gl_INTEL_map_texture

  -- * GL_INTEL_map_texture
  , glMapTexture2DINTEL
  , glSyncTextureINTEL
  , glUnmapTexture2DINTEL
  , pattern GL_LAYOUT_DEFAULT_INTEL
  , pattern GL_LAYOUT_LINEAR_CPU_CACHED_INTEL
  , pattern GL_LAYOUT_LINEAR_INTEL
  , pattern GL_TEXTURE_MEMORY_LAYOUT_INTEL
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/INTEL/map_texture.txt GL_INTEL_map_texture> extension is available.

gl_INTEL_map_texture :: Bool
gl_INTEL_map_texture = member "GL_INTEL_map_texture" extensions
{-# NOINLINE gl_INTEL_map_texture #-}

-- | Usage: @'glMapTexture2DINTEL' texture level access stride layout@
--
-- The length of @stride@ should be @1@.
--
-- The length of @layout@ should be @1@.


glMapTexture2DINTEL :: MonadIO m => GLuint -> GLint -> GLbitfield -> Ptr GLint -> Ptr GLenum -> m (Ptr ())
glMapTexture2DINTEL = ffiuintintbitfieldPtrintPtrenumIOPtrV glMapTexture2DINTELFunPtr

glMapTexture2DINTELFunPtr :: FunPtr (GLuint -> GLint -> GLbitfield -> Ptr GLint -> Ptr GLenum -> IO (Ptr ()))
glMapTexture2DINTELFunPtr = unsafePerformIO (getProcAddress "glMapTexture2DINTEL")

{-# NOINLINE glMapTexture2DINTELFunPtr #-}

-- | Usage: @'glSyncTextureINTEL' texture@


glSyncTextureINTEL :: MonadIO m => GLuint -> m ()
glSyncTextureINTEL = ffiuintIOV glSyncTextureINTELFunPtr

glSyncTextureINTELFunPtr :: FunPtr (GLuint -> IO ())
glSyncTextureINTELFunPtr = unsafePerformIO (getProcAddress "glSyncTextureINTEL")

{-# NOINLINE glSyncTextureINTELFunPtr #-}

-- | Usage: @'glUnmapTexture2DINTEL' texture level@


glUnmapTexture2DINTEL :: MonadIO m => GLuint -> GLint -> m ()
glUnmapTexture2DINTEL = ffiuintintIOV glUnmapTexture2DINTELFunPtr

glUnmapTexture2DINTELFunPtr :: FunPtr (GLuint -> GLint -> IO ())
glUnmapTexture2DINTELFunPtr = unsafePerformIO (getProcAddress "glUnmapTexture2DINTEL")

{-# NOINLINE glUnmapTexture2DINTELFunPtr #-}

pattern GL_LAYOUT_DEFAULT_INTEL  :: (Eq a, Num a) => a

pattern GL_LAYOUT_DEFAULT_INTEL = 0

pattern GL_LAYOUT_LINEAR_CPU_CACHED_INTEL  :: (Eq a, Num a) => a

pattern GL_LAYOUT_LINEAR_CPU_CACHED_INTEL = 2

pattern GL_LAYOUT_LINEAR_INTEL  :: (Eq a, Num a) => a

pattern GL_LAYOUT_LINEAR_INTEL = 1

pattern GL_TEXTURE_MEMORY_LAYOUT_INTEL  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MEMORY_LAYOUT_INTEL = 0x83FF