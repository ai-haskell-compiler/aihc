-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureCubeMap (
  -- * Extension Support
    gl_OES_texture_cube_map

  -- * GL_OES_texture_cube_map
  , glGetTexGenfvOES
  , glGetTexGenivOES
  , glGetTexGenxvOES
  , glTexGenfOES
  , glTexGenfvOES
  , glTexGeniOES
  , glTexGenivOES
  , glTexGenxOES
  , glTexGenxvOES
  , pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_OES
  , pattern GL_NORMAL_MAP_OES
  , pattern GL_REFLECTION_MAP_OES
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_OES
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_OES
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_OES
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_OES
  , pattern GL_TEXTURE_CUBE_MAP_OES
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_OES
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_OES
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_OES
  , pattern GL_TEXTURE_GEN_MODE_OES
  , pattern GL_TEXTURE_GEN_STR_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_cube_map.txt GL_OES_texture_cube_map> extension is available.

gl_OES_texture_cube_map :: Bool
gl_OES_texture_cube_map = member "GL_OES_texture_cube_map" extensions
{-# NOINLINE gl_OES_texture_cube_map #-}

-- | Usage: @'glGetTexGenfvOES' coord pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTexGenfvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetTexGenfvOES = ffienumenumPtrfloatIOV glGetTexGenfvOESFunPtr

glGetTexGenfvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetTexGenfvOESFunPtr = unsafePerformIO (getProcAddress "glGetTexGenfvOES")

{-# NOINLINE glGetTexGenfvOESFunPtr #-}

-- | Usage: @'glGetTexGenivOES' coord pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTexGenivOES :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetTexGenivOES = ffienumenumPtrintIOV glGetTexGenivOESFunPtr

glGetTexGenivOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetTexGenivOESFunPtr = unsafePerformIO (getProcAddress "glGetTexGenivOES")

{-# NOINLINE glGetTexGenivOESFunPtr #-}

-- | Usage: @'glTexGenfOES' coord pname param@


glTexGenfOES :: MonadIO m => GLenum -> GLenum -> GLfloat -> m ()
glTexGenfOES = ffienumenumfloatIOV glTexGenfOESFunPtr

glTexGenfOESFunPtr :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
glTexGenfOESFunPtr = unsafePerformIO (getProcAddress "glTexGenfOES")

{-# NOINLINE glTexGenfOESFunPtr #-}

-- | Usage: @'glTexGenfvOES' coord pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTexGenfvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glTexGenfvOES = ffienumenumPtrfloatIOV glTexGenfvOESFunPtr

glTexGenfvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glTexGenfvOESFunPtr = unsafePerformIO (getProcAddress "glTexGenfvOES")

{-# NOINLINE glTexGenfvOESFunPtr #-}

-- | Usage: @'glTexGeniOES' coord pname param@


glTexGeniOES :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glTexGeniOES = ffienumenumintIOV glTexGeniOESFunPtr

glTexGeniOESFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glTexGeniOESFunPtr = unsafePerformIO (getProcAddress "glTexGeniOES")

{-# NOINLINE glTexGeniOESFunPtr #-}

-- | Usage: @'glTexGenivOES' coord pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTexGenivOES :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glTexGenivOES = ffienumenumPtrintIOV glTexGenivOESFunPtr

glTexGenivOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glTexGenivOESFunPtr = unsafePerformIO (getProcAddress "glTexGenivOES")

{-# NOINLINE glTexGenivOESFunPtr #-}

pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_OES = 0x851C

pattern GL_NORMAL_MAP_OES  :: (Eq a, Num a) => a

pattern GL_NORMAL_MAP_OES = 0x8511

pattern GL_REFLECTION_MAP_OES  :: (Eq a, Num a) => a

pattern GL_REFLECTION_MAP_OES = 0x8512

pattern GL_TEXTURE_BINDING_CUBE_MAP_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_CUBE_MAP_OES = 0x8514

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_OES = 0x8516

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_OES = 0x8518

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_OES = 0x851A

pattern GL_TEXTURE_CUBE_MAP_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_OES = 0x8513

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_OES = 0x8515

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_OES = 0x8517

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_OES = 0x8519

pattern GL_TEXTURE_GEN_MODE_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_GEN_MODE_OES = 0x2500

pattern GL_TEXTURE_GEN_STR_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_GEN_STR_OES = 0x8D60