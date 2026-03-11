-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PixelTransform (
  -- * Extension Support
    gl_EXT_pixel_transform

  -- * GL_EXT_pixel_transform
  , glGetPixelTransformParameterfvEXT
  , glGetPixelTransformParameterivEXT
  , glPixelTransformParameterfEXT
  , glPixelTransformParameterfvEXT
  , glPixelTransformParameteriEXT
  , glPixelTransformParameterivEXT
  , pattern GL_AVERAGE_EXT
  , pattern GL_CUBIC_EXT
  , pattern GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT
  , pattern GL_PIXEL_CUBIC_WEIGHT_EXT
  , pattern GL_PIXEL_MAG_FILTER_EXT
  , pattern GL_PIXEL_MIN_FILTER_EXT
  , pattern GL_PIXEL_TRANSFORM_2D_EXT
  , pattern GL_PIXEL_TRANSFORM_2D_MATRIX_EXT
  , pattern GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/pixel_transform.txt GL_EXT_pixel_transform> extension is available.

gl_EXT_pixel_transform :: Bool
gl_EXT_pixel_transform = member "GL_EXT_pixel_transform" extensions
{-# NOINLINE gl_EXT_pixel_transform #-}

-- | Usage: @'glGetPixelTransformParameterfvEXT' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetPixelTransformParameterfvEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetPixelTransformParameterfvEXT = ffienumenumPtrfloatIOV glGetPixelTransformParameterfvEXTFunPtr

glGetPixelTransformParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetPixelTransformParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetPixelTransformParameterfvEXT")

{-# NOINLINE glGetPixelTransformParameterfvEXTFunPtr #-}

-- | Usage: @'glGetPixelTransformParameterivEXT' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetPixelTransformParameterivEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetPixelTransformParameterivEXT = ffienumenumPtrintIOV glGetPixelTransformParameterivEXTFunPtr

glGetPixelTransformParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetPixelTransformParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetPixelTransformParameterivEXT")

{-# NOINLINE glGetPixelTransformParameterivEXTFunPtr #-}

-- | Usage: @'glPixelTransformParameterfEXT' target pname param@
--
-- The parameter @target@ is a @PixelTransformTargetEXT@.
--
-- The parameter @pname@ is a @PixelTransformPNameEXT@.


glPixelTransformParameterfEXT :: MonadIO m => GLenum -> GLenum -> GLfloat -> m ()
glPixelTransformParameterfEXT = ffienumenumfloatIOV glPixelTransformParameterfEXTFunPtr

glPixelTransformParameterfEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
glPixelTransformParameterfEXTFunPtr = unsafePerformIO (getProcAddress "glPixelTransformParameterfEXT")

{-# NOINLINE glPixelTransformParameterfEXTFunPtr #-}

-- | Usage: @'glPixelTransformParameterfvEXT' target pname params@
--
-- The parameter @target@ is a @PixelTransformTargetEXT@.
--
-- The parameter @pname@ is a @PixelTransformPNameEXT@.
--
-- The length of @params@ should be @1@.


glPixelTransformParameterfvEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glPixelTransformParameterfvEXT = ffienumenumPtrfloatIOV glPixelTransformParameterfvEXTFunPtr

glPixelTransformParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glPixelTransformParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glPixelTransformParameterfvEXT")

{-# NOINLINE glPixelTransformParameterfvEXTFunPtr #-}

-- | Usage: @'glPixelTransformParameteriEXT' target pname param@
--
-- The parameter @target@ is a @PixelTransformTargetEXT@.
--
-- The parameter @pname@ is a @PixelTransformPNameEXT@.


glPixelTransformParameteriEXT :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glPixelTransformParameteriEXT = ffienumenumintIOV glPixelTransformParameteriEXTFunPtr

glPixelTransformParameteriEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glPixelTransformParameteriEXTFunPtr = unsafePerformIO (getProcAddress "glPixelTransformParameteriEXT")

{-# NOINLINE glPixelTransformParameteriEXTFunPtr #-}

-- | Usage: @'glPixelTransformParameterivEXT' target pname params@
--
-- The parameter @target@ is a @PixelTransformTargetEXT@.
--
-- The parameter @pname@ is a @PixelTransformPNameEXT@.
--
-- The length of @params@ should be @1@.


glPixelTransformParameterivEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glPixelTransformParameterivEXT = ffienumenumPtrintIOV glPixelTransformParameterivEXTFunPtr

glPixelTransformParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glPixelTransformParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glPixelTransformParameterivEXT")

{-# NOINLINE glPixelTransformParameterivEXTFunPtr #-}

pattern GL_AVERAGE_EXT  :: (Eq a, Num a) => a

pattern GL_AVERAGE_EXT = 0x8335

pattern GL_CUBIC_EXT  :: (Eq a, Num a) => a

pattern GL_CUBIC_EXT = 0x8334

pattern GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT = 0x8337

pattern GL_PIXEL_CUBIC_WEIGHT_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_CUBIC_WEIGHT_EXT = 0x8333

pattern GL_PIXEL_MAG_FILTER_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_MAG_FILTER_EXT = 0x8331

pattern GL_PIXEL_MIN_FILTER_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_MIN_FILTER_EXT = 0x8332

pattern GL_PIXEL_TRANSFORM_2D_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_TRANSFORM_2D_EXT = 0x8330

pattern GL_PIXEL_TRANSFORM_2D_MATRIX_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_TRANSFORM_2D_MATRIX_EXT = 0x8338

pattern GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT = 0x8336