-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.PixelTexture (
  -- * Extension Support
    gl_SGIS_pixel_texture

  -- * GL_SGIS_pixel_texture
  , glGetPixelTexGenParameterfvSGIS
  , glGetPixelTexGenParameterivSGIS
  , glPixelTexGenParameterfSGIS
  , glPixelTexGenParameterfvSGIS
  , glPixelTexGenParameteriSGIS
  , glPixelTexGenParameterivSGIS
  , pattern GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS
  , pattern GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS
  , pattern GL_PIXEL_GROUP_COLOR_SGIS
  , pattern GL_PIXEL_TEXTURE_SGIS
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/pixel_texture.txt GL_SGIS_pixel_texture> extension is available.

gl_SGIS_pixel_texture :: Bool
gl_SGIS_pixel_texture = member "GL_SGIS_pixel_texture" extensions
{-# NOINLINE gl_SGIS_pixel_texture #-}

-- | Usage: @'glGetPixelTexGenParameterfvSGIS' pname params@
--
-- The parameter @pname@ is a @PixelTexGenParameterNameSGIS@, one of: 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetPixelTexGenParameterfvSGIS :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glGetPixelTexGenParameterfvSGIS = ffienumPtrfloatIOV glGetPixelTexGenParameterfvSGISFunPtr

glGetPixelTexGenParameterfvSGISFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glGetPixelTexGenParameterfvSGISFunPtr = unsafePerformIO (getProcAddress "glGetPixelTexGenParameterfvSGIS")

{-# NOINLINE glGetPixelTexGenParameterfvSGISFunPtr #-}

-- | Usage: @'glGetPixelTexGenParameterivSGIS' pname params@
--
-- The parameter @pname@ is a @PixelTexGenParameterNameSGIS@, one of: 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetPixelTexGenParameterivSGIS :: MonadIO m => GLenum -> Ptr GLint -> m ()
glGetPixelTexGenParameterivSGIS = ffienumPtrintIOV glGetPixelTexGenParameterivSGISFunPtr

glGetPixelTexGenParameterivSGISFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glGetPixelTexGenParameterivSGISFunPtr = unsafePerformIO (getProcAddress "glGetPixelTexGenParameterivSGIS")

{-# NOINLINE glGetPixelTexGenParameterivSGISFunPtr #-}

-- | Usage: @'glPixelTexGenParameterfSGIS' pname param@
--
-- The parameter @pname@ is a @PixelTexGenParameterNameSGIS@, one of: 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS'.
--
-- The parameter @param@ is a @CheckedFloat32@.


glPixelTexGenParameterfSGIS :: MonadIO m => GLenum -> GLfloat -> m ()
glPixelTexGenParameterfSGIS = ffienumfloatIOV glPixelTexGenParameterfSGISFunPtr

glPixelTexGenParameterfSGISFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glPixelTexGenParameterfSGISFunPtr = unsafePerformIO (getProcAddress "glPixelTexGenParameterfSGIS")

{-# NOINLINE glPixelTexGenParameterfSGISFunPtr #-}

-- | Usage: @'glPixelTexGenParameterfvSGIS' pname params@
--
-- The parameter @pname@ is a @PixelTexGenParameterNameSGIS@, one of: 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glPixelTexGenParameterfvSGIS :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glPixelTexGenParameterfvSGIS = ffienumPtrfloatIOV glPixelTexGenParameterfvSGISFunPtr

glPixelTexGenParameterfvSGISFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glPixelTexGenParameterfvSGISFunPtr = unsafePerformIO (getProcAddress "glPixelTexGenParameterfvSGIS")

{-# NOINLINE glPixelTexGenParameterfvSGISFunPtr #-}

-- | Usage: @'glPixelTexGenParameteriSGIS' pname param@
--
-- The parameter @pname@ is a @PixelTexGenParameterNameSGIS@, one of: 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS'.
--
-- The parameter @param@ is a @CheckedInt32@.


glPixelTexGenParameteriSGIS :: MonadIO m => GLenum -> GLint -> m ()
glPixelTexGenParameteriSGIS = ffienumintIOV glPixelTexGenParameteriSGISFunPtr

glPixelTexGenParameteriSGISFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glPixelTexGenParameteriSGISFunPtr = unsafePerformIO (getProcAddress "glPixelTexGenParameteriSGIS")

{-# NOINLINE glPixelTexGenParameteriSGISFunPtr #-}

-- | Usage: @'glPixelTexGenParameterivSGIS' pname params@
--
-- The parameter @pname@ is a @PixelTexGenParameterNameSGIS@, one of: 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glPixelTexGenParameterivSGIS :: MonadIO m => GLenum -> Ptr GLint -> m ()
glPixelTexGenParameterivSGIS = ffienumPtrintIOV glPixelTexGenParameterivSGISFunPtr

glPixelTexGenParameterivSGISFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glPixelTexGenParameterivSGISFunPtr = unsafePerformIO (getProcAddress "glPixelTexGenParameterivSGIS")

{-# NOINLINE glPixelTexGenParameterivSGISFunPtr #-}

pattern GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS  :: (Eq a, Num a) => a

pattern GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS = 0x8355

pattern GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS  :: (Eq a, Num a) => a

pattern GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS = 0x8354

pattern GL_PIXEL_GROUP_COLOR_SGIS  :: (Eq a, Num a) => a

pattern GL_PIXEL_GROUP_COLOR_SGIS = 0x8356

pattern GL_PIXEL_TEXTURE_SGIS  :: (Eq a, Num a) => a

pattern GL_PIXEL_TEXTURE_SGIS = 0x8353