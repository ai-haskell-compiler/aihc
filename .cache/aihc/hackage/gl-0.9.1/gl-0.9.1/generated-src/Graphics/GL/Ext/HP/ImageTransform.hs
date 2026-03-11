-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.HP.ImageTransform (
  -- * Extension Support
    gl_HP_image_transform

  -- * GL_HP_image_transform
  , glGetImageTransformParameterfvHP
  , glGetImageTransformParameterivHP
  , glImageTransformParameterfHP
  , glImageTransformParameterfvHP
  , glImageTransformParameteriHP
  , glImageTransformParameterivHP
  , pattern GL_AVERAGE_HP
  , pattern GL_CUBIC_HP
  , pattern GL_IMAGE_CUBIC_WEIGHT_HP
  , pattern GL_IMAGE_MAG_FILTER_HP
  , pattern GL_IMAGE_MIN_FILTER_HP
  , pattern GL_IMAGE_ROTATE_ANGLE_HP
  , pattern GL_IMAGE_ROTATE_ORIGIN_X_HP
  , pattern GL_IMAGE_ROTATE_ORIGIN_Y_HP
  , pattern GL_IMAGE_SCALE_X_HP
  , pattern GL_IMAGE_SCALE_Y_HP
  , pattern GL_IMAGE_TRANSFORM_2D_HP
  , pattern GL_IMAGE_TRANSLATE_X_HP
  , pattern GL_IMAGE_TRANSLATE_Y_HP
  , pattern GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP
  , pattern GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/HP/image_transform.txt GL_HP_image_transform> extension is available.

gl_HP_image_transform :: Bool
gl_HP_image_transform = member "GL_HP_image_transform" extensions
{-# NOINLINE gl_HP_image_transform #-}

-- | Usage: @'glGetImageTransformParameterfvHP' target pname params@
--
-- The parameter @target@ is a @ImageTransformTargetHP@.
--
-- The parameter @pname@ is a @ImageTransformPNameHP@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetImageTransformParameterfvHP :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetImageTransformParameterfvHP = ffienumenumPtrfloatIOV glGetImageTransformParameterfvHPFunPtr

glGetImageTransformParameterfvHPFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetImageTransformParameterfvHPFunPtr = unsafePerformIO (getProcAddress "glGetImageTransformParameterfvHP")

{-# NOINLINE glGetImageTransformParameterfvHPFunPtr #-}

-- | Usage: @'glGetImageTransformParameterivHP' target pname params@
--
-- The parameter @target@ is a @ImageTransformTargetHP@.
--
-- The parameter @pname@ is a @ImageTransformPNameHP@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetImageTransformParameterivHP :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetImageTransformParameterivHP = ffienumenumPtrintIOV glGetImageTransformParameterivHPFunPtr

glGetImageTransformParameterivHPFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetImageTransformParameterivHPFunPtr = unsafePerformIO (getProcAddress "glGetImageTransformParameterivHP")

{-# NOINLINE glGetImageTransformParameterivHPFunPtr #-}

-- | Usage: @'glImageTransformParameterfHP' target pname param@
--
-- The parameter @target@ is a @ImageTransformTargetHP@.
--
-- The parameter @pname@ is a @ImageTransformPNameHP@.


glImageTransformParameterfHP :: MonadIO m => GLenum -> GLenum -> GLfloat -> m ()
glImageTransformParameterfHP = ffienumenumfloatIOV glImageTransformParameterfHPFunPtr

glImageTransformParameterfHPFunPtr :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
glImageTransformParameterfHPFunPtr = unsafePerformIO (getProcAddress "glImageTransformParameterfHP")

{-# NOINLINE glImageTransformParameterfHPFunPtr #-}

-- | Usage: @'glImageTransformParameterfvHP' target pname params@
--
-- The parameter @target@ is a @ImageTransformTargetHP@.
--
-- The parameter @pname@ is a @ImageTransformPNameHP@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glImageTransformParameterfvHP :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glImageTransformParameterfvHP = ffienumenumPtrfloatIOV glImageTransformParameterfvHPFunPtr

glImageTransformParameterfvHPFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glImageTransformParameterfvHPFunPtr = unsafePerformIO (getProcAddress "glImageTransformParameterfvHP")

{-# NOINLINE glImageTransformParameterfvHPFunPtr #-}

-- | Usage: @'glImageTransformParameteriHP' target pname param@
--
-- The parameter @target@ is a @ImageTransformTargetHP@.
--
-- The parameter @pname@ is a @ImageTransformPNameHP@.


glImageTransformParameteriHP :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glImageTransformParameteriHP = ffienumenumintIOV glImageTransformParameteriHPFunPtr

glImageTransformParameteriHPFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glImageTransformParameteriHPFunPtr = unsafePerformIO (getProcAddress "glImageTransformParameteriHP")

{-# NOINLINE glImageTransformParameteriHPFunPtr #-}

-- | Usage: @'glImageTransformParameterivHP' target pname params@
--
-- The parameter @target@ is a @ImageTransformTargetHP@.
--
-- The parameter @pname@ is a @ImageTransformPNameHP@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glImageTransformParameterivHP :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glImageTransformParameterivHP = ffienumenumPtrintIOV glImageTransformParameterivHPFunPtr

glImageTransformParameterivHPFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glImageTransformParameterivHPFunPtr = unsafePerformIO (getProcAddress "glImageTransformParameterivHP")

{-# NOINLINE glImageTransformParameterivHPFunPtr #-}

pattern GL_AVERAGE_HP  :: (Eq a, Num a) => a

pattern GL_AVERAGE_HP = 0x8160

pattern GL_CUBIC_HP  :: (Eq a, Num a) => a

pattern GL_CUBIC_HP = 0x815F

pattern GL_IMAGE_CUBIC_WEIGHT_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_CUBIC_WEIGHT_HP = 0x815E

pattern GL_IMAGE_MAG_FILTER_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_MAG_FILTER_HP = 0x815C

pattern GL_IMAGE_MIN_FILTER_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_MIN_FILTER_HP = 0x815D

pattern GL_IMAGE_ROTATE_ANGLE_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_ROTATE_ANGLE_HP = 0x8159

pattern GL_IMAGE_ROTATE_ORIGIN_X_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_ROTATE_ORIGIN_X_HP = 0x815A

pattern GL_IMAGE_ROTATE_ORIGIN_Y_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_ROTATE_ORIGIN_Y_HP = 0x815B

pattern GL_IMAGE_SCALE_X_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_SCALE_X_HP = 0x8155

pattern GL_IMAGE_SCALE_Y_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_SCALE_Y_HP = 0x8156

pattern GL_IMAGE_TRANSFORM_2D_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_TRANSFORM_2D_HP = 0x8161

pattern GL_IMAGE_TRANSLATE_X_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_TRANSLATE_X_HP = 0x8157

pattern GL_IMAGE_TRANSLATE_Y_HP  :: (Eq a, Num a) => a

pattern GL_IMAGE_TRANSLATE_Y_HP = 0x8158

pattern GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP  :: (Eq a, Num a) => a

pattern GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP = 0x8162

pattern GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP  :: (Eq a, Num a) => a

pattern GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP = 0x8163