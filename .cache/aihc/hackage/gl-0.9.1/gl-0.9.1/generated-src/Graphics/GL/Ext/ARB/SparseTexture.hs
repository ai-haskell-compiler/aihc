-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SparseTexture (
  -- * Extension Support
    gl_ARB_sparse_texture

  -- * GL_ARB_sparse_texture
  , glTexPageCommitmentARB
  , pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB
  , pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_ARB
  , pattern GL_MAX_SPARSE_TEXTURE_SIZE_ARB
  , pattern GL_NUM_SPARSE_LEVELS_ARB
  , pattern GL_NUM_VIRTUAL_PAGE_SIZES_ARB
  , pattern GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_ARB
  , pattern GL_TEXTURE_SPARSE_ARB
  , pattern GL_VIRTUAL_PAGE_SIZE_INDEX_ARB
  , pattern GL_VIRTUAL_PAGE_SIZE_X_ARB
  , pattern GL_VIRTUAL_PAGE_SIZE_Y_ARB
  , pattern GL_VIRTUAL_PAGE_SIZE_Z_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/sparse_texture.txt GL_ARB_sparse_texture> extension is available.

gl_ARB_sparse_texture :: Bool
gl_ARB_sparse_texture = member "GL_ARB_sparse_texture" extensions
{-# NOINLINE gl_ARB_sparse_texture #-}

-- | Usage: @'glTexPageCommitmentARB' target level xoffset yoffset zoffset width height depth commit@


glTexPageCommitmentARB :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
glTexPageCommitmentARB = ffienumintintintintsizeisizeisizeibooleanIOV glTexPageCommitmentARBFunPtr

glTexPageCommitmentARBFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTexPageCommitmentARBFunPtr = unsafePerformIO (getProcAddress "glTexPageCommitmentARB")

{-# NOINLINE glTexPageCommitmentARBFunPtr #-}

pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB = 0x9199

pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_ARB = 0x919A

pattern GL_MAX_SPARSE_TEXTURE_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_TEXTURE_SIZE_ARB = 0x9198

pattern GL_NUM_SPARSE_LEVELS_ARB  :: (Eq a, Num a) => a

pattern GL_NUM_SPARSE_LEVELS_ARB = 0x91AA

pattern GL_NUM_VIRTUAL_PAGE_SIZES_ARB  :: (Eq a, Num a) => a

pattern GL_NUM_VIRTUAL_PAGE_SIZES_ARB = 0x91A8

pattern GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_ARB  :: (Eq a, Num a) => a

pattern GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_ARB = 0x91A9

pattern GL_TEXTURE_SPARSE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SPARSE_ARB = 0x91A6

pattern GL_VIRTUAL_PAGE_SIZE_INDEX_ARB  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_INDEX_ARB = 0x91A7

pattern GL_VIRTUAL_PAGE_SIZE_X_ARB  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_X_ARB = 0x9195

pattern GL_VIRTUAL_PAGE_SIZE_Y_ARB  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_Y_ARB = 0x9196

pattern GL_VIRTUAL_PAGE_SIZE_Z_ARB  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_Z_ARB = 0x9197