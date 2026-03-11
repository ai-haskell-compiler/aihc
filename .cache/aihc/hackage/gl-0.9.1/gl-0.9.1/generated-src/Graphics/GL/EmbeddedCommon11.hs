-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.EmbeddedCommon11 (
  -- * Graphics.GL.EmbeddedCommon11
    module Graphics.GL.EmbeddedLite11
  , glAlphaFunc
  , glClearColor
  , glClearDepthf
  , glClipPlanef
  , glColor4f
  , glDepthRangef
  , glFogf
  , glFogfv
  , glFrustumf
  , glGetClipPlanef
  , glGetFloatv
  , glGetLightfv
  , glGetMaterialfv
  , glGetTexEnvfv
  , glGetTexParameterfv
  , glLightModelf
  , glLightModelfv
  , glLightf
  , glLightfv
  , glLineWidth
  , glLoadMatrixf
  , glMaterialf
  , glMaterialfv
  , glMultMatrixf
  , glMultiTexCoord4f
  , glNormal3f
  , glOrthof
  , glPointParameterf
  , glPointParameterfv
  , glPointSize
  , glPolygonOffset
  , glRotatef
  , glScalef
  , glTexEnvf
  , glTexEnvfv
  , glTexParameterf
  , glTexParameterfv
  , glTranslatef
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

import Graphics.GL.EmbeddedLite11

-- | Usage: @'glClipPlanef' p eqn@
--
-- The length of @eqn@ should be @4@.


glClipPlanef :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glClipPlanef = ffienumPtrfloatIOV glClipPlanefFunPtr

glClipPlanefFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glClipPlanefFunPtr = unsafePerformIO (getProcAddress "glClipPlanef")

{-# NOINLINE glClipPlanefFunPtr #-}

-- | Usage: @'glFrustumf' l r b t n f@


glFrustumf :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glFrustumf = ffifloatfloatfloatfloatfloatfloatIOV glFrustumfFunPtr

glFrustumfFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glFrustumfFunPtr = unsafePerformIO (getProcAddress "glFrustumf")

{-# NOINLINE glFrustumfFunPtr #-}

-- | Usage: @'glGetClipPlanef' plane equation@
--
-- The length of @equation@ should be @4@.


glGetClipPlanef :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glGetClipPlanef = ffienumPtrfloatIOV glGetClipPlanefFunPtr

glGetClipPlanefFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glGetClipPlanefFunPtr = unsafePerformIO (getProcAddress "glGetClipPlanef")

{-# NOINLINE glGetClipPlanefFunPtr #-}

-- | Usage: @'glOrthof' l r b t n f@


glOrthof :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glOrthof = ffifloatfloatfloatfloatfloatfloatIOV glOrthofFunPtr

glOrthofFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glOrthofFunPtr = unsafePerformIO (getProcAddress "glOrthof")

{-# NOINLINE glOrthofFunPtr #-}