-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.SparseTexture (
  -- * Extension Support
    gl_AMD_sparse_texture

  -- * GL_AMD_sparse_texture
  , glTexStorageSparseAMD
  , glTextureStorageSparseAMD
  , pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_AMD
  , pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS
  , pattern GL_MAX_SPARSE_TEXTURE_SIZE_AMD
  , pattern GL_MIN_LOD_WARNING_AMD
  , pattern GL_MIN_SPARSE_LEVEL_AMD
  , pattern GL_TEXTURE_STORAGE_SPARSE_BIT_AMD
  , pattern GL_VIRTUAL_PAGE_SIZE_X_AMD
  , pattern GL_VIRTUAL_PAGE_SIZE_Y_AMD
  , pattern GL_VIRTUAL_PAGE_SIZE_Z_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/sparse_texture.txt GL_AMD_sparse_texture> extension is available.

gl_AMD_sparse_texture :: Bool
gl_AMD_sparse_texture = member "GL_AMD_sparse_texture" extensions
{-# NOINLINE gl_AMD_sparse_texture #-}

-- | Usage: @'glTexStorageSparseAMD' target internalFormat width height depth layers flags@


glTexStorageSparseAMD :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> m ()
glTexStorageSparseAMD = ffienumenumsizeisizeisizeisizeibitfieldIOV glTexStorageSparseAMDFunPtr

glTexStorageSparseAMDFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ())
glTexStorageSparseAMDFunPtr = unsafePerformIO (getProcAddress "glTexStorageSparseAMD")

{-# NOINLINE glTexStorageSparseAMDFunPtr #-}

-- | Usage: @'glTextureStorageSparseAMD' texture target internalFormat width height depth layers flags@


glTextureStorageSparseAMD :: MonadIO m => GLuint -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> m ()
glTextureStorageSparseAMD = ffiuintenumenumsizeisizeisizeisizeibitfieldIOV glTextureStorageSparseAMDFunPtr

glTextureStorageSparseAMDFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ())
glTextureStorageSparseAMDFunPtr = unsafePerformIO (getProcAddress "glTextureStorageSparseAMD")

{-# NOINLINE glTextureStorageSparseAMDFunPtr #-}

pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_AMD  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_AMD = 0x9199

pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS = 0x919A

pattern GL_MAX_SPARSE_TEXTURE_SIZE_AMD  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_TEXTURE_SIZE_AMD = 0x9198

pattern GL_MIN_LOD_WARNING_AMD  :: (Eq a, Num a) => a

pattern GL_MIN_LOD_WARNING_AMD = 0x919C

pattern GL_MIN_SPARSE_LEVEL_AMD  :: (Eq a, Num a) => a

pattern GL_MIN_SPARSE_LEVEL_AMD = 0x919B

pattern GL_TEXTURE_STORAGE_SPARSE_BIT_AMD  :: (Eq a, Num a) => a

pattern GL_TEXTURE_STORAGE_SPARSE_BIT_AMD = 0x00000001

pattern GL_VIRTUAL_PAGE_SIZE_X_AMD  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_X_AMD = 0x9195

pattern GL_VIRTUAL_PAGE_SIZE_Y_AMD  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_Y_AMD = 0x9196

pattern GL_VIRTUAL_PAGE_SIZE_Z_AMD  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_Z_AMD = 0x9197