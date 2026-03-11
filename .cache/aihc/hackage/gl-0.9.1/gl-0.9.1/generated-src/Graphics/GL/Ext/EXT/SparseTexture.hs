-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.SparseTexture (
  -- * Extension Support
    gl_EXT_sparse_texture

  -- * GL_EXT_sparse_texture
  , glTexPageCommitmentEXT
  , pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_EXT
  , pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_EXT
  , pattern GL_MAX_SPARSE_TEXTURE_SIZE_EXT
  , pattern GL_NUM_SPARSE_LEVELS_EXT
  , pattern GL_NUM_VIRTUAL_PAGE_SIZES_EXT
  , pattern GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_EXT
  , pattern GL_TEXTURE_2D
  , pattern GL_TEXTURE_2D_ARRAY
  , pattern GL_TEXTURE_3D
  , pattern GL_TEXTURE_CUBE_MAP
  , pattern GL_TEXTURE_CUBE_MAP_ARRAY_OES
  , pattern GL_TEXTURE_SPARSE_EXT
  , pattern GL_VIRTUAL_PAGE_SIZE_INDEX_EXT
  , pattern GL_VIRTUAL_PAGE_SIZE_X_EXT
  , pattern GL_VIRTUAL_PAGE_SIZE_Y_EXT
  , pattern GL_VIRTUAL_PAGE_SIZE_Z_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_sparse_texture.txt GL_EXT_sparse_texture> extension is available.

gl_EXT_sparse_texture :: Bool
gl_EXT_sparse_texture = member "GL_EXT_sparse_texture" extensions
{-# NOINLINE gl_EXT_sparse_texture #-}

-- | Usage: @'glTexPageCommitmentEXT' target level xoffset yoffset zoffset width height depth commit@
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.SparseTexture.glTexPageCommitmentARB'.


glTexPageCommitmentEXT :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
glTexPageCommitmentEXT = ffienumintintintintsizeisizeisizeibooleanIOV glTexPageCommitmentEXTFunPtr

glTexPageCommitmentEXTFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTexPageCommitmentEXTFunPtr = unsafePerformIO (getProcAddress "glTexPageCommitmentEXT")

{-# NOINLINE glTexPageCommitmentEXTFunPtr #-}

pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_EXT = 0x9199

pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_EXT = 0x919A

pattern GL_MAX_SPARSE_TEXTURE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_SPARSE_TEXTURE_SIZE_EXT = 0x9198

pattern GL_NUM_SPARSE_LEVELS_EXT  :: (Eq a, Num a) => a

pattern GL_NUM_SPARSE_LEVELS_EXT = 0x91AA

pattern GL_NUM_VIRTUAL_PAGE_SIZES_EXT  :: (Eq a, Num a) => a

pattern GL_NUM_VIRTUAL_PAGE_SIZES_EXT = 0x91A8

pattern GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_EXT  :: (Eq a, Num a) => a

pattern GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_EXT = 0x91A9

pattern GL_TEXTURE_SPARSE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SPARSE_EXT = 0x91A6

pattern GL_VIRTUAL_PAGE_SIZE_INDEX_EXT  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_INDEX_EXT = 0x91A7

pattern GL_VIRTUAL_PAGE_SIZE_X_EXT  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_X_EXT = 0x9195

pattern GL_VIRTUAL_PAGE_SIZE_Y_EXT  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_Y_EXT = 0x9196

pattern GL_VIRTUAL_PAGE_SIZE_Z_EXT  :: (Eq a, Num a) => a

pattern GL_VIRTUAL_PAGE_SIZE_Z_EXT = 0x9197