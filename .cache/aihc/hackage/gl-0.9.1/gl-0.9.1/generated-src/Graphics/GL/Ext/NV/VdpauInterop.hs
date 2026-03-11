-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VdpauInterop (
  -- * Extension Support
    gl_NV_vdpau_interop

  -- * GL_NV_vdpau_interop
  , glVDPAUFiniNV
  , glVDPAUGetSurfaceivNV
  , glVDPAUInitNV
  , glVDPAUIsSurfaceNV
  , glVDPAUMapSurfacesNV
  , glVDPAURegisterOutputSurfaceNV
  , glVDPAURegisterVideoSurfaceNV
  , glVDPAUSurfaceAccessNV
  , glVDPAUUnmapSurfacesNV
  , glVDPAUUnregisterSurfaceNV
  , pattern GL_SURFACE_MAPPED_NV
  , pattern GL_SURFACE_REGISTERED_NV
  , pattern GL_SURFACE_STATE_NV
  , pattern GL_WRITE_DISCARD_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vdpau_interop.txt GL_NV_vdpau_interop> extension is available.

gl_NV_vdpau_interop :: Bool
gl_NV_vdpau_interop = member "GL_NV_vdpau_interop" extensions
{-# NOINLINE gl_NV_vdpau_interop #-}

-- | Usage: @'glVDPAUFiniNV'@


glVDPAUFiniNV :: MonadIO m => m ()
glVDPAUFiniNV = ffiIOV glVDPAUFiniNVFunPtr

glVDPAUFiniNVFunPtr :: FunPtr (IO ())
glVDPAUFiniNVFunPtr = unsafePerformIO (getProcAddress "glVDPAUFiniNV")

{-# NOINLINE glVDPAUFiniNVFunPtr #-}

-- | Usage: @'glVDPAUGetSurfaceivNV' surface pname bufSize length values@
--
-- The parameter @surface@ is a @vdpauSurfaceNV@.
--
-- The length of @values@ should be @bufSize@.


glVDPAUGetSurfaceivNV :: MonadIO m => GLvdpauSurfaceNV -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> m ()
glVDPAUGetSurfaceivNV = ffivdpauSurfaceNVenumsizeiPtrsizeiPtrintIOV glVDPAUGetSurfaceivNVFunPtr

glVDPAUGetSurfaceivNVFunPtr :: FunPtr (GLvdpauSurfaceNV -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
glVDPAUGetSurfaceivNVFunPtr = unsafePerformIO (getProcAddress "glVDPAUGetSurfaceivNV")

{-# NOINLINE glVDPAUGetSurfaceivNVFunPtr #-}

-- | Usage: @'glVDPAUInitNV' vdpDevice getProcAddress@


glVDPAUInitNV :: MonadIO m => Ptr () -> Ptr () -> m ()
glVDPAUInitNV = ffiPtrVPtrVIOV glVDPAUInitNVFunPtr

glVDPAUInitNVFunPtr :: FunPtr (Ptr () -> Ptr () -> IO ())
glVDPAUInitNVFunPtr = unsafePerformIO (getProcAddress "glVDPAUInitNV")

{-# NOINLINE glVDPAUInitNVFunPtr #-}

-- | Usage: @'glVDPAUIsSurfaceNV' surface@
--
-- The parameter @surface@ is a @vdpauSurfaceNV@.


glVDPAUIsSurfaceNV :: MonadIO m => GLvdpauSurfaceNV -> m GLboolean
glVDPAUIsSurfaceNV = ffivdpauSurfaceNVIOboolean glVDPAUIsSurfaceNVFunPtr

glVDPAUIsSurfaceNVFunPtr :: FunPtr (GLvdpauSurfaceNV -> IO GLboolean)
glVDPAUIsSurfaceNVFunPtr = unsafePerformIO (getProcAddress "glVDPAUIsSurfaceNV")

{-# NOINLINE glVDPAUIsSurfaceNVFunPtr #-}

-- | Usage: @'glVDPAUMapSurfacesNV' numSurfaces surfaces@
--
-- The parameter @surfaces@ is a @vdpauSurfaceNV@.
--
-- The length of @surfaces@ should be @numSurfaces@.


glVDPAUMapSurfacesNV :: MonadIO m => GLsizei -> Ptr GLvdpauSurfaceNV -> m ()
glVDPAUMapSurfacesNV = ffisizeiPtrvdpauSurfaceNVIOV glVDPAUMapSurfacesNVFunPtr

glVDPAUMapSurfacesNVFunPtr :: FunPtr (GLsizei -> Ptr GLvdpauSurfaceNV -> IO ())
glVDPAUMapSurfacesNVFunPtr = unsafePerformIO (getProcAddress "glVDPAUMapSurfacesNV")

{-# NOINLINE glVDPAUMapSurfacesNVFunPtr #-}

-- | Usage: @'glVDPAURegisterOutputSurfaceNV' vdpSurface target numTextureNames textureNames@
--
-- The length of @textureNames@ should be @numTextureNames@.


glVDPAURegisterOutputSurfaceNV :: MonadIO m => Ptr () -> GLenum -> GLsizei -> Ptr GLuint -> m GLvdpauSurfaceNV
glVDPAURegisterOutputSurfaceNV = ffiPtrVenumsizeiPtruintIOvdpauSurfaceNV glVDPAURegisterOutputSurfaceNVFunPtr

glVDPAURegisterOutputSurfaceNVFunPtr :: FunPtr (Ptr () -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV)
glVDPAURegisterOutputSurfaceNVFunPtr = unsafePerformIO (getProcAddress "glVDPAURegisterOutputSurfaceNV")

{-# NOINLINE glVDPAURegisterOutputSurfaceNVFunPtr #-}

-- | Usage: @'glVDPAURegisterVideoSurfaceNV' vdpSurface target numTextureNames textureNames@
--
-- The length of @textureNames@ should be @numTextureNames@.


glVDPAURegisterVideoSurfaceNV :: MonadIO m => Ptr () -> GLenum -> GLsizei -> Ptr GLuint -> m GLvdpauSurfaceNV
glVDPAURegisterVideoSurfaceNV = ffiPtrVenumsizeiPtruintIOvdpauSurfaceNV glVDPAURegisterVideoSurfaceNVFunPtr

glVDPAURegisterVideoSurfaceNVFunPtr :: FunPtr (Ptr () -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV)
glVDPAURegisterVideoSurfaceNVFunPtr = unsafePerformIO (getProcAddress "glVDPAURegisterVideoSurfaceNV")

{-# NOINLINE glVDPAURegisterVideoSurfaceNVFunPtr #-}

-- | Usage: @'glVDPAUSurfaceAccessNV' surface access@
--
-- The parameter @surface@ is a @vdpauSurfaceNV@.


glVDPAUSurfaceAccessNV :: MonadIO m => GLvdpauSurfaceNV -> GLenum -> m ()
glVDPAUSurfaceAccessNV = ffivdpauSurfaceNVenumIOV glVDPAUSurfaceAccessNVFunPtr

glVDPAUSurfaceAccessNVFunPtr :: FunPtr (GLvdpauSurfaceNV -> GLenum -> IO ())
glVDPAUSurfaceAccessNVFunPtr = unsafePerformIO (getProcAddress "glVDPAUSurfaceAccessNV")

{-# NOINLINE glVDPAUSurfaceAccessNVFunPtr #-}

-- | Usage: @'glVDPAUUnmapSurfacesNV' numSurface surfaces@
--
-- The parameter @surfaces@ is a @vdpauSurfaceNV@.
--
-- The length of @surfaces@ should be @numSurface@.


glVDPAUUnmapSurfacesNV :: MonadIO m => GLsizei -> Ptr GLvdpauSurfaceNV -> m ()
glVDPAUUnmapSurfacesNV = ffisizeiPtrvdpauSurfaceNVIOV glVDPAUUnmapSurfacesNVFunPtr

glVDPAUUnmapSurfacesNVFunPtr :: FunPtr (GLsizei -> Ptr GLvdpauSurfaceNV -> IO ())
glVDPAUUnmapSurfacesNVFunPtr = unsafePerformIO (getProcAddress "glVDPAUUnmapSurfacesNV")

{-# NOINLINE glVDPAUUnmapSurfacesNVFunPtr #-}

-- | Usage: @'glVDPAUUnregisterSurfaceNV' surface@
--
-- The parameter @surface@ is a @vdpauSurfaceNV@.


glVDPAUUnregisterSurfaceNV :: MonadIO m => GLvdpauSurfaceNV -> m ()
glVDPAUUnregisterSurfaceNV = ffivdpauSurfaceNVIOV glVDPAUUnregisterSurfaceNVFunPtr

glVDPAUUnregisterSurfaceNVFunPtr :: FunPtr (GLvdpauSurfaceNV -> IO ())
glVDPAUUnregisterSurfaceNVFunPtr = unsafePerformIO (getProcAddress "glVDPAUUnregisterSurfaceNV")

{-# NOINLINE glVDPAUUnregisterSurfaceNVFunPtr #-}

pattern GL_SURFACE_MAPPED_NV  :: (Eq a, Num a) => a

pattern GL_SURFACE_MAPPED_NV = 0x8700

pattern GL_SURFACE_REGISTERED_NV  :: (Eq a, Num a) => a

pattern GL_SURFACE_REGISTERED_NV = 0x86FD

pattern GL_SURFACE_STATE_NV  :: (Eq a, Num a) => a

pattern GL_SURFACE_STATE_NV = 0x86EB

pattern GL_WRITE_DISCARD_NV  :: (Eq a, Num a) => a

pattern GL_WRITE_DISCARD_NV = 0x88BE