-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.FixedPoint (
  -- * Extension Support
    gl_OES_fixed_point

  -- * GL_OES_fixed_point
  , glAccumxOES
  , glAlphaFuncxOES
  , glBitmapxOES
  , glBlendColorxOES
  , glClearAccumxOES
  , glClearColorxOES
  , glClearDepthxOES
  , glClipPlanexOES
  , glColor3xOES
  , glColor3xvOES
  , glColor4xOES
  , glColor4xvOES
  , glConvolutionParameterxOES
  , glConvolutionParameterxvOES
  , glDepthRangexOES
  , glEvalCoord1xOES
  , glEvalCoord1xvOES
  , glEvalCoord2xOES
  , glEvalCoord2xvOES
  , glFeedbackBufferxOES
  , glFogxOES
  , glFogxvOES
  , glFrustumxOES
  , glGetClipPlanexOES
  , glGetConvolutionParameterxvOES
  , glGetFixedvOES
  , glGetHistogramParameterxvOES
  , glGetLightxOES
  , glGetLightxvOES
  , glGetMapxvOES
  , glGetMaterialxOES
  , glGetMaterialxvOES
  , glGetPixelMapxv
  , glGetTexEnvxvOES
  , glGetTexGenxvOES
  , glGetTexLevelParameterxvOES
  , glGetTexParameterxvOES
  , glIndexxOES
  , glIndexxvOES
  , glLightModelxOES
  , glLightModelxvOES
  , glLightxOES
  , glLightxvOES
  , glLineWidthxOES
  , glLoadMatrixxOES
  , glLoadTransposeMatrixxOES
  , glMap1xOES
  , glMap2xOES
  , glMapGrid1xOES
  , glMapGrid2xOES
  , glMaterialxOES
  , glMaterialxvOES
  , glMultMatrixxOES
  , glMultTransposeMatrixxOES
  , glMultiTexCoord1xOES
  , glMultiTexCoord1xvOES
  , glMultiTexCoord2xOES
  , glMultiTexCoord2xvOES
  , glMultiTexCoord3xOES
  , glMultiTexCoord3xvOES
  , glMultiTexCoord4xOES
  , glMultiTexCoord4xvOES
  , glNormal3xOES
  , glNormal3xvOES
  , glOrthoxOES
  , glPassThroughxOES
  , glPixelMapx
  , glPixelStorex
  , glPixelTransferxOES
  , glPixelZoomxOES
  , glPointParameterxOES
  , glPointParameterxvOES
  , glPointSizexOES
  , glPolygonOffsetxOES
  , glPrioritizeTexturesxOES
  , glRasterPos2xOES
  , glRasterPos2xvOES
  , glRasterPos3xOES
  , glRasterPos3xvOES
  , glRasterPos4xOES
  , glRasterPos4xvOES
  , glRectxOES
  , glRectxvOES
  , glRotatexOES
  , glSampleCoveragexOES
  , glScalexOES
  , glTexCoord1xOES
  , glTexCoord1xvOES
  , glTexCoord2xOES
  , glTexCoord2xvOES
  , glTexCoord3xOES
  , glTexCoord3xvOES
  , glTexCoord4xOES
  , glTexCoord4xvOES
  , glTexEnvxOES
  , glTexEnvxvOES
  , glTexGenxOES
  , glTexGenxvOES
  , glTexParameterxOES
  , glTexParameterxvOES
  , glTranslatexOES
  , glVertex2xOES
  , glVertex2xvOES
  , glVertex3xOES
  , glVertex3xvOES
  , glVertex4xOES
  , glVertex4xvOES
  , pattern GL_FIXED_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_fixed_point.txt GL_OES_fixed_point> extension is available.

gl_OES_fixed_point :: Bool
gl_OES_fixed_point = member "GL_OES_fixed_point" extensions
{-# NOINLINE gl_OES_fixed_point #-}

-- | Usage: @'glAccumxOES' op value@


glAccumxOES :: MonadIO m => GLenum -> GLfixed -> m ()
glAccumxOES = ffienumfixedIOV glAccumxOESFunPtr

glAccumxOESFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glAccumxOESFunPtr = unsafePerformIO (getProcAddress "glAccumxOES")

{-# NOINLINE glAccumxOESFunPtr #-}

-- | Usage: @'glAlphaFuncxOES' func ref@
--
-- The parameter @ref@ is a @ClampedFixed@.


glAlphaFuncxOES :: MonadIO m => GLenum -> GLfixed -> m ()
glAlphaFuncxOES = ffienumfixedIOV glAlphaFuncxOESFunPtr

glAlphaFuncxOESFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glAlphaFuncxOESFunPtr = unsafePerformIO (getProcAddress "glAlphaFuncxOES")

{-# NOINLINE glAlphaFuncxOESFunPtr #-}

-- | Usage: @'glBitmapxOES' width height xorig yorig xmove ymove bitmap@
--
-- The length of @bitmap@ should be @COMPSIZE(width,height)@.


glBitmapxOES :: MonadIO m => GLsizei -> GLsizei -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> Ptr GLubyte -> m ()
glBitmapxOES = ffisizeisizeifixedfixedfixedfixedPtrubyteIOV glBitmapxOESFunPtr

glBitmapxOESFunPtr :: FunPtr (GLsizei -> GLsizei -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> Ptr GLubyte -> IO ())
glBitmapxOESFunPtr = unsafePerformIO (getProcAddress "glBitmapxOES")

{-# NOINLINE glBitmapxOESFunPtr #-}

-- | Usage: @'glBlendColorxOES' red green blue alpha@
--
-- The parameter @red@ is a @ClampedFixed@.
--
-- The parameter @green@ is a @ClampedFixed@.
--
-- The parameter @blue@ is a @ClampedFixed@.
--
-- The parameter @alpha@ is a @ClampedFixed@.


glBlendColorxOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glBlendColorxOES = ffifixedfixedfixedfixedIOV glBlendColorxOESFunPtr

glBlendColorxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glBlendColorxOESFunPtr = unsafePerformIO (getProcAddress "glBlendColorxOES")

{-# NOINLINE glBlendColorxOESFunPtr #-}

-- | Usage: @'glClearAccumxOES' red green blue alpha@
--
-- The parameter @red@ is a @ClampedFixed@.
--
-- The parameter @green@ is a @ClampedFixed@.
--
-- The parameter @blue@ is a @ClampedFixed@.
--
-- The parameter @alpha@ is a @ClampedFixed@.


glClearAccumxOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glClearAccumxOES = ffifixedfixedfixedfixedIOV glClearAccumxOESFunPtr

glClearAccumxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glClearAccumxOESFunPtr = unsafePerformIO (getProcAddress "glClearAccumxOES")

{-# NOINLINE glClearAccumxOESFunPtr #-}

-- | Usage: @'glClearColorxOES' red green blue alpha@
--
-- The parameter @red@ is a @ClampedFixed@.
--
-- The parameter @green@ is a @ClampedFixed@.
--
-- The parameter @blue@ is a @ClampedFixed@.
--
-- The parameter @alpha@ is a @ClampedFixed@.


glClearColorxOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glClearColorxOES = ffifixedfixedfixedfixedIOV glClearColorxOESFunPtr

glClearColorxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glClearColorxOESFunPtr = unsafePerformIO (getProcAddress "glClearColorxOES")

{-# NOINLINE glClearColorxOESFunPtr #-}

-- | Usage: @'glClearDepthxOES' depth@
--
-- The parameter @depth@ is a @ClampedFixed@.


glClearDepthxOES :: MonadIO m => GLfixed -> m ()
glClearDepthxOES = ffifixedIOV glClearDepthxOESFunPtr

glClearDepthxOESFunPtr :: FunPtr (GLfixed -> IO ())
glClearDepthxOESFunPtr = unsafePerformIO (getProcAddress "glClearDepthxOES")

{-# NOINLINE glClearDepthxOESFunPtr #-}

-- | Usage: @'glClipPlanexOES' plane equation@
--
-- The length of @equation@ should be @4@.


glClipPlanexOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glClipPlanexOES = ffienumPtrfixedIOV glClipPlanexOESFunPtr

glClipPlanexOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glClipPlanexOESFunPtr = unsafePerformIO (getProcAddress "glClipPlanexOES")

{-# NOINLINE glClipPlanexOESFunPtr #-}

-- | Usage: @'glColor3xOES' red green blue@


glColor3xOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glColor3xOES = ffifixedfixedfixedIOV glColor3xOESFunPtr

glColor3xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glColor3xOESFunPtr = unsafePerformIO (getProcAddress "glColor3xOES")

{-# NOINLINE glColor3xOESFunPtr #-}

-- | Usage: @'glColor3xvOES' components@
--
-- The length of @components@ should be @3@.


glColor3xvOES :: MonadIO m => Ptr GLfixed -> m ()
glColor3xvOES = ffiPtrfixedIOV glColor3xvOESFunPtr

glColor3xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glColor3xvOESFunPtr = unsafePerformIO (getProcAddress "glColor3xvOES")

{-# NOINLINE glColor3xvOESFunPtr #-}

-- | Usage: @'glColor4xOES' red green blue alpha@


glColor4xOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glColor4xOES = ffifixedfixedfixedfixedIOV glColor4xOESFunPtr

glColor4xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glColor4xOESFunPtr = unsafePerformIO (getProcAddress "glColor4xOES")

{-# NOINLINE glColor4xOESFunPtr #-}

-- | Usage: @'glColor4xvOES' components@
--
-- The length of @components@ should be @4@.


glColor4xvOES :: MonadIO m => Ptr GLfixed -> m ()
glColor4xvOES = ffiPtrfixedIOV glColor4xvOESFunPtr

glColor4xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glColor4xvOESFunPtr = unsafePerformIO (getProcAddress "glColor4xvOES")

{-# NOINLINE glColor4xvOESFunPtr #-}

-- | Usage: @'glConvolutionParameterxOES' target pname param@


glConvolutionParameterxOES :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glConvolutionParameterxOES = ffienumenumfixedIOV glConvolutionParameterxOESFunPtr

glConvolutionParameterxOESFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glConvolutionParameterxOESFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameterxOES")

{-# NOINLINE glConvolutionParameterxOESFunPtr #-}

-- | Usage: @'glConvolutionParameterxvOES' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glConvolutionParameterxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glConvolutionParameterxvOES = ffienumenumPtrfixedIOV glConvolutionParameterxvOESFunPtr

glConvolutionParameterxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glConvolutionParameterxvOESFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameterxvOES")

{-# NOINLINE glConvolutionParameterxvOESFunPtr #-}

-- | Usage: @'glDepthRangexOES' n f@
--
-- The parameter @n@ is a @ClampedFixed@.
--
-- The parameter @f@ is a @ClampedFixed@.


glDepthRangexOES :: MonadIO m => GLfixed -> GLfixed -> m ()
glDepthRangexOES = ffifixedfixedIOV glDepthRangexOESFunPtr

glDepthRangexOESFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glDepthRangexOESFunPtr = unsafePerformIO (getProcAddress "glDepthRangexOES")

{-# NOINLINE glDepthRangexOESFunPtr #-}

-- | Usage: @'glEvalCoord1xOES' u@


glEvalCoord1xOES :: MonadIO m => GLfixed -> m ()
glEvalCoord1xOES = ffifixedIOV glEvalCoord1xOESFunPtr

glEvalCoord1xOESFunPtr :: FunPtr (GLfixed -> IO ())
glEvalCoord1xOESFunPtr = unsafePerformIO (getProcAddress "glEvalCoord1xOES")

{-# NOINLINE glEvalCoord1xOESFunPtr #-}

-- | Usage: @'glEvalCoord1xvOES' coords@
--
-- The length of @coords@ should be @1@.


glEvalCoord1xvOES :: MonadIO m => Ptr GLfixed -> m ()
glEvalCoord1xvOES = ffiPtrfixedIOV glEvalCoord1xvOESFunPtr

glEvalCoord1xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glEvalCoord1xvOESFunPtr = unsafePerformIO (getProcAddress "glEvalCoord1xvOES")

{-# NOINLINE glEvalCoord1xvOESFunPtr #-}

-- | Usage: @'glEvalCoord2xOES' u v@


glEvalCoord2xOES :: MonadIO m => GLfixed -> GLfixed -> m ()
glEvalCoord2xOES = ffifixedfixedIOV glEvalCoord2xOESFunPtr

glEvalCoord2xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glEvalCoord2xOESFunPtr = unsafePerformIO (getProcAddress "glEvalCoord2xOES")

{-# NOINLINE glEvalCoord2xOESFunPtr #-}

-- | Usage: @'glEvalCoord2xvOES' coords@
--
-- The length of @coords@ should be @2@.


glEvalCoord2xvOES :: MonadIO m => Ptr GLfixed -> m ()
glEvalCoord2xvOES = ffiPtrfixedIOV glEvalCoord2xvOESFunPtr

glEvalCoord2xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glEvalCoord2xvOESFunPtr = unsafePerformIO (getProcAddress "glEvalCoord2xvOES")

{-# NOINLINE glEvalCoord2xvOESFunPtr #-}

-- | Usage: @'glFeedbackBufferxOES' n type buffer@
--
-- The length of @buffer@ should be @n@.


glFeedbackBufferxOES :: MonadIO m => GLsizei -> GLenum -> Ptr GLfixed -> m ()
glFeedbackBufferxOES = ffisizeienumPtrfixedIOV glFeedbackBufferxOESFunPtr

glFeedbackBufferxOESFunPtr :: FunPtr (GLsizei -> GLenum -> Ptr GLfixed -> IO ())
glFeedbackBufferxOESFunPtr = unsafePerformIO (getProcAddress "glFeedbackBufferxOES")

{-# NOINLINE glFeedbackBufferxOESFunPtr #-}

-- | Usage: @'glFogxOES' pname param@


glFogxOES :: MonadIO m => GLenum -> GLfixed -> m ()
glFogxOES = ffienumfixedIOV glFogxOESFunPtr

glFogxOESFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glFogxOESFunPtr = unsafePerformIO (getProcAddress "glFogxOES")

{-# NOINLINE glFogxOESFunPtr #-}

-- | Usage: @'glFogxvOES' pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glFogxvOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glFogxvOES = ffienumPtrfixedIOV glFogxvOESFunPtr

glFogxvOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glFogxvOESFunPtr = unsafePerformIO (getProcAddress "glFogxvOES")

{-# NOINLINE glFogxvOESFunPtr #-}

-- | Usage: @'glFrustumxOES' l r b t n f@


glFrustumxOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glFrustumxOES = ffifixedfixedfixedfixedfixedfixedIOV glFrustumxOESFunPtr

glFrustumxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glFrustumxOESFunPtr = unsafePerformIO (getProcAddress "glFrustumxOES")

{-# NOINLINE glFrustumxOESFunPtr #-}

-- | Usage: @'glGetClipPlanexOES' plane equation@
--
-- The length of @equation@ should be @4@.


glGetClipPlanexOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glGetClipPlanexOES = ffienumPtrfixedIOV glGetClipPlanexOESFunPtr

glGetClipPlanexOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glGetClipPlanexOESFunPtr = unsafePerformIO (getProcAddress "glGetClipPlanexOES")

{-# NOINLINE glGetClipPlanexOESFunPtr #-}

-- | Usage: @'glGetConvolutionParameterxvOES' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetConvolutionParameterxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetConvolutionParameterxvOES = ffienumenumPtrfixedIOV glGetConvolutionParameterxvOESFunPtr

glGetConvolutionParameterxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetConvolutionParameterxvOESFunPtr = unsafePerformIO (getProcAddress "glGetConvolutionParameterxvOES")

{-# NOINLINE glGetConvolutionParameterxvOESFunPtr #-}

-- | Usage: @'glGetFixedvOES' pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFixedvOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glGetFixedvOES = ffienumPtrfixedIOV glGetFixedvOESFunPtr

glGetFixedvOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glGetFixedvOESFunPtr = unsafePerformIO (getProcAddress "glGetFixedvOES")

{-# NOINLINE glGetFixedvOESFunPtr #-}

-- | Usage: @'glGetHistogramParameterxvOES' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetHistogramParameterxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetHistogramParameterxvOES = ffienumenumPtrfixedIOV glGetHistogramParameterxvOESFunPtr

glGetHistogramParameterxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetHistogramParameterxvOESFunPtr = unsafePerformIO (getProcAddress "glGetHistogramParameterxvOES")

{-# NOINLINE glGetHistogramParameterxvOESFunPtr #-}

-- | Usage: @'glGetLightxOES' light pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetLightxOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetLightxOES = ffienumenumPtrfixedIOV glGetLightxOESFunPtr

glGetLightxOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetLightxOESFunPtr = unsafePerformIO (getProcAddress "glGetLightxOES")

{-# NOINLINE glGetLightxOESFunPtr #-}

-- | Usage: @'glGetLightxvOES' light pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetLightxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetLightxvOES = ffienumenumPtrfixedIOV glGetLightxvOESFunPtr

glGetLightxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetLightxvOESFunPtr = unsafePerformIO (getProcAddress "glGetLightxvOES")

{-# NOINLINE glGetLightxvOESFunPtr #-}

-- | Usage: @'glGetMapxvOES' target query v@
--
-- The length of @v@ should be @COMPSIZE(query)@.


glGetMapxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetMapxvOES = ffienumenumPtrfixedIOV glGetMapxvOESFunPtr

glGetMapxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetMapxvOESFunPtr = unsafePerformIO (getProcAddress "glGetMapxvOES")

{-# NOINLINE glGetMapxvOESFunPtr #-}

-- | Usage: @'glGetMaterialxOES' face pname param@


glGetMaterialxOES :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glGetMaterialxOES = ffienumenumfixedIOV glGetMaterialxOESFunPtr

glGetMaterialxOESFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glGetMaterialxOESFunPtr = unsafePerformIO (getProcAddress "glGetMaterialxOES")

{-# NOINLINE glGetMaterialxOESFunPtr #-}

-- | Usage: @'glGetMaterialxvOES' face pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMaterialxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetMaterialxvOES = ffienumenumPtrfixedIOV glGetMaterialxvOESFunPtr

glGetMaterialxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetMaterialxvOESFunPtr = unsafePerformIO (getProcAddress "glGetMaterialxvOES")

{-# NOINLINE glGetMaterialxvOESFunPtr #-}

-- | Usage: @'glGetPixelMapxv' map size values@
--
-- The length of @values@ should be @size@.


glGetPixelMapxv :: MonadIO m => GLenum -> GLint -> Ptr GLfixed -> m ()
glGetPixelMapxv = ffienumintPtrfixedIOV glGetPixelMapxvFunPtr

glGetPixelMapxvFunPtr :: FunPtr (GLenum -> GLint -> Ptr GLfixed -> IO ())
glGetPixelMapxvFunPtr = unsafePerformIO (getProcAddress "glGetPixelMapxv")

{-# NOINLINE glGetPixelMapxvFunPtr #-}

-- | Usage: @'glGetTexEnvxvOES' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTexEnvxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetTexEnvxvOES = ffienumenumPtrfixedIOV glGetTexEnvxvOESFunPtr

glGetTexEnvxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetTexEnvxvOESFunPtr = unsafePerformIO (getProcAddress "glGetTexEnvxvOES")

{-# NOINLINE glGetTexEnvxvOESFunPtr #-}

-- | Usage: @'glGetTexLevelParameterxvOES' target level pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTexLevelParameterxvOES :: MonadIO m => GLenum -> GLint -> GLenum -> Ptr GLfixed -> m ()
glGetTexLevelParameterxvOES = ffienumintenumPtrfixedIOV glGetTexLevelParameterxvOESFunPtr

glGetTexLevelParameterxvOESFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfixed -> IO ())
glGetTexLevelParameterxvOESFunPtr = unsafePerformIO (getProcAddress "glGetTexLevelParameterxvOES")

{-# NOINLINE glGetTexLevelParameterxvOESFunPtr #-}

-- | Usage: @'glGetTexParameterxvOES' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTexParameterxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetTexParameterxvOES = ffienumenumPtrfixedIOV glGetTexParameterxvOESFunPtr

glGetTexParameterxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetTexParameterxvOESFunPtr = unsafePerformIO (getProcAddress "glGetTexParameterxvOES")

{-# NOINLINE glGetTexParameterxvOESFunPtr #-}

-- | Usage: @'glIndexxOES' component@


glIndexxOES :: MonadIO m => GLfixed -> m ()
glIndexxOES = ffifixedIOV glIndexxOESFunPtr

glIndexxOESFunPtr :: FunPtr (GLfixed -> IO ())
glIndexxOESFunPtr = unsafePerformIO (getProcAddress "glIndexxOES")

{-# NOINLINE glIndexxOESFunPtr #-}

-- | Usage: @'glIndexxvOES' component@
--
-- The length of @component@ should be @1@.


glIndexxvOES :: MonadIO m => Ptr GLfixed -> m ()
glIndexxvOES = ffiPtrfixedIOV glIndexxvOESFunPtr

glIndexxvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glIndexxvOESFunPtr = unsafePerformIO (getProcAddress "glIndexxvOES")

{-# NOINLINE glIndexxvOESFunPtr #-}

-- | Usage: @'glLightModelxOES' pname param@


glLightModelxOES :: MonadIO m => GLenum -> GLfixed -> m ()
glLightModelxOES = ffienumfixedIOV glLightModelxOESFunPtr

glLightModelxOESFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glLightModelxOESFunPtr = unsafePerformIO (getProcAddress "glLightModelxOES")

{-# NOINLINE glLightModelxOESFunPtr #-}

-- | Usage: @'glLightModelxvOES' pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glLightModelxvOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glLightModelxvOES = ffienumPtrfixedIOV glLightModelxvOESFunPtr

glLightModelxvOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glLightModelxvOESFunPtr = unsafePerformIO (getProcAddress "glLightModelxvOES")

{-# NOINLINE glLightModelxvOESFunPtr #-}

-- | Usage: @'glLightxOES' light pname param@


glLightxOES :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glLightxOES = ffienumenumfixedIOV glLightxOESFunPtr

glLightxOESFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glLightxOESFunPtr = unsafePerformIO (getProcAddress "glLightxOES")

{-# NOINLINE glLightxOESFunPtr #-}

-- | Usage: @'glLightxvOES' light pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glLightxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glLightxvOES = ffienumenumPtrfixedIOV glLightxvOESFunPtr

glLightxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glLightxvOESFunPtr = unsafePerformIO (getProcAddress "glLightxvOES")

{-# NOINLINE glLightxvOESFunPtr #-}

-- | Usage: @'glLineWidthxOES' width@


glLineWidthxOES :: MonadIO m => GLfixed -> m ()
glLineWidthxOES = ffifixedIOV glLineWidthxOESFunPtr

glLineWidthxOESFunPtr :: FunPtr (GLfixed -> IO ())
glLineWidthxOESFunPtr = unsafePerformIO (getProcAddress "glLineWidthxOES")

{-# NOINLINE glLineWidthxOESFunPtr #-}

-- | Usage: @'glLoadMatrixxOES' m@
--
-- The length of @m@ should be @16@.


glLoadMatrixxOES :: MonadIO m => Ptr GLfixed -> m ()
glLoadMatrixxOES = ffiPtrfixedIOV glLoadMatrixxOESFunPtr

glLoadMatrixxOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glLoadMatrixxOESFunPtr = unsafePerformIO (getProcAddress "glLoadMatrixxOES")

{-# NOINLINE glLoadMatrixxOESFunPtr #-}

-- | Usage: @'glLoadTransposeMatrixxOES' m@
--
-- The length of @m@ should be @16@.


glLoadTransposeMatrixxOES :: MonadIO m => Ptr GLfixed -> m ()
glLoadTransposeMatrixxOES = ffiPtrfixedIOV glLoadTransposeMatrixxOESFunPtr

glLoadTransposeMatrixxOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glLoadTransposeMatrixxOESFunPtr = unsafePerformIO (getProcAddress "glLoadTransposeMatrixxOES")

{-# NOINLINE glLoadTransposeMatrixxOESFunPtr #-}

-- | Usage: @'glMap1xOES' target u1 u2 stride order points@


glMap1xOES :: MonadIO m => GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> m ()
glMap1xOES = ffienumfixedfixedintintfixedIOV glMap1xOESFunPtr

glMap1xOESFunPtr :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ())
glMap1xOESFunPtr = unsafePerformIO (getProcAddress "glMap1xOES")

{-# NOINLINE glMap1xOESFunPtr #-}

-- | Usage: @'glMap2xOES' target u1 u2 ustride uorder v1 v2 vstride vorder points@


glMap2xOES :: MonadIO m => GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> m ()
glMap2xOES = ffienumfixedfixedintintfixedfixedintintfixedIOV glMap2xOESFunPtr

glMap2xOESFunPtr :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ())
glMap2xOESFunPtr = unsafePerformIO (getProcAddress "glMap2xOES")

{-# NOINLINE glMap2xOESFunPtr #-}

-- | Usage: @'glMapGrid1xOES' n u1 u2@


glMapGrid1xOES :: MonadIO m => GLint -> GLfixed -> GLfixed -> m ()
glMapGrid1xOES = ffiintfixedfixedIOV glMapGrid1xOESFunPtr

glMapGrid1xOESFunPtr :: FunPtr (GLint -> GLfixed -> GLfixed -> IO ())
glMapGrid1xOESFunPtr = unsafePerformIO (getProcAddress "glMapGrid1xOES")

{-# NOINLINE glMapGrid1xOESFunPtr #-}

-- | Usage: @'glMapGrid2xOES' n u1 u2 v1 v2@


glMapGrid2xOES :: MonadIO m => GLint -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glMapGrid2xOES = ffiintfixedfixedfixedfixedIOV glMapGrid2xOESFunPtr

glMapGrid2xOESFunPtr :: FunPtr (GLint -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glMapGrid2xOESFunPtr = unsafePerformIO (getProcAddress "glMapGrid2xOES")

{-# NOINLINE glMapGrid2xOESFunPtr #-}

-- | Usage: @'glMaterialxOES' face pname param@


glMaterialxOES :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glMaterialxOES = ffienumenumfixedIOV glMaterialxOESFunPtr

glMaterialxOESFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glMaterialxOESFunPtr = unsafePerformIO (getProcAddress "glMaterialxOES")

{-# NOINLINE glMaterialxOESFunPtr #-}

-- | Usage: @'glMaterialxvOES' face pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glMaterialxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glMaterialxvOES = ffienumenumPtrfixedIOV glMaterialxvOESFunPtr

glMaterialxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glMaterialxvOESFunPtr = unsafePerformIO (getProcAddress "glMaterialxvOES")

{-# NOINLINE glMaterialxvOESFunPtr #-}

-- | Usage: @'glMultMatrixxOES' m@
--
-- The length of @m@ should be @16@.


glMultMatrixxOES :: MonadIO m => Ptr GLfixed -> m ()
glMultMatrixxOES = ffiPtrfixedIOV glMultMatrixxOESFunPtr

glMultMatrixxOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glMultMatrixxOESFunPtr = unsafePerformIO (getProcAddress "glMultMatrixxOES")

{-# NOINLINE glMultMatrixxOESFunPtr #-}

-- | Usage: @'glMultTransposeMatrixxOES' m@
--
-- The length of @m@ should be @16@.


glMultTransposeMatrixxOES :: MonadIO m => Ptr GLfixed -> m ()
glMultTransposeMatrixxOES = ffiPtrfixedIOV glMultTransposeMatrixxOESFunPtr

glMultTransposeMatrixxOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glMultTransposeMatrixxOESFunPtr = unsafePerformIO (getProcAddress "glMultTransposeMatrixxOES")

{-# NOINLINE glMultTransposeMatrixxOESFunPtr #-}

-- | Usage: @'glMultiTexCoord1xOES' texture s@


glMultiTexCoord1xOES :: MonadIO m => GLenum -> GLfixed -> m ()
glMultiTexCoord1xOES = ffienumfixedIOV glMultiTexCoord1xOESFunPtr

glMultiTexCoord1xOESFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glMultiTexCoord1xOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1xOES")

{-# NOINLINE glMultiTexCoord1xOESFunPtr #-}

-- | Usage: @'glMultiTexCoord1xvOES' texture coords@
--
-- The length of @coords@ should be @1@.


glMultiTexCoord1xvOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glMultiTexCoord1xvOES = ffienumPtrfixedIOV glMultiTexCoord1xvOESFunPtr

glMultiTexCoord1xvOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glMultiTexCoord1xvOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1xvOES")

{-# NOINLINE glMultiTexCoord1xvOESFunPtr #-}

-- | Usage: @'glMultiTexCoord2xOES' texture s t@


glMultiTexCoord2xOES :: MonadIO m => GLenum -> GLfixed -> GLfixed -> m ()
glMultiTexCoord2xOES = ffienumfixedfixedIOV glMultiTexCoord2xOESFunPtr

glMultiTexCoord2xOESFunPtr :: FunPtr (GLenum -> GLfixed -> GLfixed -> IO ())
glMultiTexCoord2xOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2xOES")

{-# NOINLINE glMultiTexCoord2xOESFunPtr #-}

-- | Usage: @'glMultiTexCoord2xvOES' texture coords@
--
-- The length of @coords@ should be @2@.


glMultiTexCoord2xvOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glMultiTexCoord2xvOES = ffienumPtrfixedIOV glMultiTexCoord2xvOESFunPtr

glMultiTexCoord2xvOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glMultiTexCoord2xvOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2xvOES")

{-# NOINLINE glMultiTexCoord2xvOESFunPtr #-}

-- | Usage: @'glMultiTexCoord3xOES' texture s t r@


glMultiTexCoord3xOES :: MonadIO m => GLenum -> GLfixed -> GLfixed -> GLfixed -> m ()
glMultiTexCoord3xOES = ffienumfixedfixedfixedIOV glMultiTexCoord3xOESFunPtr

glMultiTexCoord3xOESFunPtr :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> IO ())
glMultiTexCoord3xOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3xOES")

{-# NOINLINE glMultiTexCoord3xOESFunPtr #-}

-- | Usage: @'glMultiTexCoord3xvOES' texture coords@
--
-- The length of @coords@ should be @3@.


glMultiTexCoord3xvOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glMultiTexCoord3xvOES = ffienumPtrfixedIOV glMultiTexCoord3xvOESFunPtr

glMultiTexCoord3xvOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glMultiTexCoord3xvOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3xvOES")

{-# NOINLINE glMultiTexCoord3xvOESFunPtr #-}

-- | Usage: @'glMultiTexCoord4xOES' texture s t r q@


glMultiTexCoord4xOES :: MonadIO m => GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glMultiTexCoord4xOES = ffienumfixedfixedfixedfixedIOV glMultiTexCoord4xOESFunPtr

glMultiTexCoord4xOESFunPtr :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glMultiTexCoord4xOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4xOES")

{-# NOINLINE glMultiTexCoord4xOESFunPtr #-}

-- | Usage: @'glMultiTexCoord4xvOES' texture coords@
--
-- The length of @coords@ should be @4@.


glMultiTexCoord4xvOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glMultiTexCoord4xvOES = ffienumPtrfixedIOV glMultiTexCoord4xvOESFunPtr

glMultiTexCoord4xvOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glMultiTexCoord4xvOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4xvOES")

{-# NOINLINE glMultiTexCoord4xvOESFunPtr #-}

-- | Usage: @'glNormal3xOES' nx ny nz@


glNormal3xOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glNormal3xOES = ffifixedfixedfixedIOV glNormal3xOESFunPtr

glNormal3xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glNormal3xOESFunPtr = unsafePerformIO (getProcAddress "glNormal3xOES")

{-# NOINLINE glNormal3xOESFunPtr #-}

-- | Usage: @'glNormal3xvOES' coords@
--
-- The length of @coords@ should be @3@.


glNormal3xvOES :: MonadIO m => Ptr GLfixed -> m ()
glNormal3xvOES = ffiPtrfixedIOV glNormal3xvOESFunPtr

glNormal3xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glNormal3xvOESFunPtr = unsafePerformIO (getProcAddress "glNormal3xvOES")

{-# NOINLINE glNormal3xvOESFunPtr #-}

-- | Usage: @'glOrthoxOES' l r b t n f@


glOrthoxOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glOrthoxOES = ffifixedfixedfixedfixedfixedfixedIOV glOrthoxOESFunPtr

glOrthoxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glOrthoxOESFunPtr = unsafePerformIO (getProcAddress "glOrthoxOES")

{-# NOINLINE glOrthoxOESFunPtr #-}

-- | Usage: @'glPassThroughxOES' token@


glPassThroughxOES :: MonadIO m => GLfixed -> m ()
glPassThroughxOES = ffifixedIOV glPassThroughxOESFunPtr

glPassThroughxOESFunPtr :: FunPtr (GLfixed -> IO ())
glPassThroughxOESFunPtr = unsafePerformIO (getProcAddress "glPassThroughxOES")

{-# NOINLINE glPassThroughxOESFunPtr #-}

-- | Usage: @'glPixelMapx' map size values@
--
-- The length of @values@ should be @size@.


glPixelMapx :: MonadIO m => GLenum -> GLint -> Ptr GLfixed -> m ()
glPixelMapx = ffienumintPtrfixedIOV glPixelMapxFunPtr

glPixelMapxFunPtr :: FunPtr (GLenum -> GLint -> Ptr GLfixed -> IO ())
glPixelMapxFunPtr = unsafePerformIO (getProcAddress "glPixelMapx")

{-# NOINLINE glPixelMapxFunPtr #-}

-- | Usage: @'glPixelStorex' pname param@


glPixelStorex :: MonadIO m => GLenum -> GLfixed -> m ()
glPixelStorex = ffienumfixedIOV glPixelStorexFunPtr

glPixelStorexFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glPixelStorexFunPtr = unsafePerformIO (getProcAddress "glPixelStorex")

{-# NOINLINE glPixelStorexFunPtr #-}

-- | Usage: @'glPixelTransferxOES' pname param@


glPixelTransferxOES :: MonadIO m => GLenum -> GLfixed -> m ()
glPixelTransferxOES = ffienumfixedIOV glPixelTransferxOESFunPtr

glPixelTransferxOESFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glPixelTransferxOESFunPtr = unsafePerformIO (getProcAddress "glPixelTransferxOES")

{-# NOINLINE glPixelTransferxOESFunPtr #-}

-- | Usage: @'glPixelZoomxOES' xfactor yfactor@


glPixelZoomxOES :: MonadIO m => GLfixed -> GLfixed -> m ()
glPixelZoomxOES = ffifixedfixedIOV glPixelZoomxOESFunPtr

glPixelZoomxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glPixelZoomxOESFunPtr = unsafePerformIO (getProcAddress "glPixelZoomxOES")

{-# NOINLINE glPixelZoomxOESFunPtr #-}

-- | Usage: @'glPointParameterxOES' pname param@


glPointParameterxOES :: MonadIO m => GLenum -> GLfixed -> m ()
glPointParameterxOES = ffienumfixedIOV glPointParameterxOESFunPtr

glPointParameterxOESFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glPointParameterxOESFunPtr = unsafePerformIO (getProcAddress "glPointParameterxOES")

{-# NOINLINE glPointParameterxOESFunPtr #-}

-- | Usage: @'glPointParameterxvOES' pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glPointParameterxvOES :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glPointParameterxvOES = ffienumPtrfixedIOV glPointParameterxvOESFunPtr

glPointParameterxvOESFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glPointParameterxvOESFunPtr = unsafePerformIO (getProcAddress "glPointParameterxvOES")

{-# NOINLINE glPointParameterxvOESFunPtr #-}

-- | Usage: @'glPointSizexOES' size@


glPointSizexOES :: MonadIO m => GLfixed -> m ()
glPointSizexOES = ffifixedIOV glPointSizexOESFunPtr

glPointSizexOESFunPtr :: FunPtr (GLfixed -> IO ())
glPointSizexOESFunPtr = unsafePerformIO (getProcAddress "glPointSizexOES")

{-# NOINLINE glPointSizexOESFunPtr #-}

-- | Usage: @'glPolygonOffsetxOES' factor units@


glPolygonOffsetxOES :: MonadIO m => GLfixed -> GLfixed -> m ()
glPolygonOffsetxOES = ffifixedfixedIOV glPolygonOffsetxOESFunPtr

glPolygonOffsetxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glPolygonOffsetxOESFunPtr = unsafePerformIO (getProcAddress "glPolygonOffsetxOES")

{-# NOINLINE glPolygonOffsetxOESFunPtr #-}

-- | Usage: @'glPrioritizeTexturesxOES' n textures priorities@
--
-- The parameter @priorities@ is a @ClampedFixed@.
--
-- The length of @textures@ should be @n@.
--
-- The length of @priorities@ should be @n@.


glPrioritizeTexturesxOES :: MonadIO m => GLsizei -> Ptr GLuint -> Ptr GLfixed -> m ()
glPrioritizeTexturesxOES = ffisizeiPtruintPtrfixedIOV glPrioritizeTexturesxOESFunPtr

glPrioritizeTexturesxOESFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLfixed -> IO ())
glPrioritizeTexturesxOESFunPtr = unsafePerformIO (getProcAddress "glPrioritizeTexturesxOES")

{-# NOINLINE glPrioritizeTexturesxOESFunPtr #-}

-- | Usage: @'glRasterPos2xOES' x y@


glRasterPos2xOES :: MonadIO m => GLfixed -> GLfixed -> m ()
glRasterPos2xOES = ffifixedfixedIOV glRasterPos2xOESFunPtr

glRasterPos2xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glRasterPos2xOESFunPtr = unsafePerformIO (getProcAddress "glRasterPos2xOES")

{-# NOINLINE glRasterPos2xOESFunPtr #-}

-- | Usage: @'glRasterPos2xvOES' coords@
--
-- The length of @coords@ should be @2@.


glRasterPos2xvOES :: MonadIO m => Ptr GLfixed -> m ()
glRasterPos2xvOES = ffiPtrfixedIOV glRasterPos2xvOESFunPtr

glRasterPos2xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glRasterPos2xvOESFunPtr = unsafePerformIO (getProcAddress "glRasterPos2xvOES")

{-# NOINLINE glRasterPos2xvOESFunPtr #-}

-- | Usage: @'glRasterPos3xOES' x y z@


glRasterPos3xOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glRasterPos3xOES = ffifixedfixedfixedIOV glRasterPos3xOESFunPtr

glRasterPos3xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glRasterPos3xOESFunPtr = unsafePerformIO (getProcAddress "glRasterPos3xOES")

{-# NOINLINE glRasterPos3xOESFunPtr #-}

-- | Usage: @'glRasterPos3xvOES' coords@
--
-- The length of @coords@ should be @3@.


glRasterPos3xvOES :: MonadIO m => Ptr GLfixed -> m ()
glRasterPos3xvOES = ffiPtrfixedIOV glRasterPos3xvOESFunPtr

glRasterPos3xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glRasterPos3xvOESFunPtr = unsafePerformIO (getProcAddress "glRasterPos3xvOES")

{-# NOINLINE glRasterPos3xvOESFunPtr #-}

-- | Usage: @'glRasterPos4xOES' x y z w@


glRasterPos4xOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glRasterPos4xOES = ffifixedfixedfixedfixedIOV glRasterPos4xOESFunPtr

glRasterPos4xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glRasterPos4xOESFunPtr = unsafePerformIO (getProcAddress "glRasterPos4xOES")

{-# NOINLINE glRasterPos4xOESFunPtr #-}

-- | Usage: @'glRasterPos4xvOES' coords@
--
-- The length of @coords@ should be @4@.


glRasterPos4xvOES :: MonadIO m => Ptr GLfixed -> m ()
glRasterPos4xvOES = ffiPtrfixedIOV glRasterPos4xvOESFunPtr

glRasterPos4xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glRasterPos4xvOESFunPtr = unsafePerformIO (getProcAddress "glRasterPos4xvOES")

{-# NOINLINE glRasterPos4xvOESFunPtr #-}

-- | Usage: @'glRectxOES' x1 y1 x2 y2@


glRectxOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glRectxOES = ffifixedfixedfixedfixedIOV glRectxOESFunPtr

glRectxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glRectxOESFunPtr = unsafePerformIO (getProcAddress "glRectxOES")

{-# NOINLINE glRectxOESFunPtr #-}

-- | Usage: @'glRectxvOES' v1 v2@
--
-- The length of @v1@ should be @2@.
--
-- The length of @v2@ should be @2@.


glRectxvOES :: MonadIO m => Ptr GLfixed -> Ptr GLfixed -> m ()
glRectxvOES = ffiPtrfixedPtrfixedIOV glRectxvOESFunPtr

glRectxvOESFunPtr :: FunPtr (Ptr GLfixed -> Ptr GLfixed -> IO ())
glRectxvOESFunPtr = unsafePerformIO (getProcAddress "glRectxvOES")

{-# NOINLINE glRectxvOESFunPtr #-}

-- | Usage: @'glRotatexOES' angle x y z@


glRotatexOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glRotatexOES = ffifixedfixedfixedfixedIOV glRotatexOESFunPtr

glRotatexOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glRotatexOESFunPtr = unsafePerformIO (getProcAddress "glRotatexOES")

{-# NOINLINE glRotatexOESFunPtr #-}

-- | Usage: @'glSampleCoveragexOES' value invert@


glSampleCoveragexOES :: MonadIO m => GLclampx -> GLboolean -> m ()
glSampleCoveragexOES = fficlampxbooleanIOV glSampleCoveragexOESFunPtr

glSampleCoveragexOESFunPtr :: FunPtr (GLclampx -> GLboolean -> IO ())
glSampleCoveragexOESFunPtr = unsafePerformIO (getProcAddress "glSampleCoveragexOES")

{-# NOINLINE glSampleCoveragexOESFunPtr #-}

-- | Usage: @'glScalexOES' x y z@


glScalexOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glScalexOES = ffifixedfixedfixedIOV glScalexOESFunPtr

glScalexOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glScalexOESFunPtr = unsafePerformIO (getProcAddress "glScalexOES")

{-# NOINLINE glScalexOESFunPtr #-}

-- | Usage: @'glTexCoord1xOES' s@


glTexCoord1xOES :: MonadIO m => GLfixed -> m ()
glTexCoord1xOES = ffifixedIOV glTexCoord1xOESFunPtr

glTexCoord1xOESFunPtr :: FunPtr (GLfixed -> IO ())
glTexCoord1xOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord1xOES")

{-# NOINLINE glTexCoord1xOESFunPtr #-}

-- | Usage: @'glTexCoord1xvOES' coords@
--
-- The length of @coords@ should be @1@.


glTexCoord1xvOES :: MonadIO m => Ptr GLfixed -> m ()
glTexCoord1xvOES = ffiPtrfixedIOV glTexCoord1xvOESFunPtr

glTexCoord1xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glTexCoord1xvOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord1xvOES")

{-# NOINLINE glTexCoord1xvOESFunPtr #-}

-- | Usage: @'glTexCoord2xOES' s t@


glTexCoord2xOES :: MonadIO m => GLfixed -> GLfixed -> m ()
glTexCoord2xOES = ffifixedfixedIOV glTexCoord2xOESFunPtr

glTexCoord2xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glTexCoord2xOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord2xOES")

{-# NOINLINE glTexCoord2xOESFunPtr #-}

-- | Usage: @'glTexCoord2xvOES' coords@
--
-- The length of @coords@ should be @2@.


glTexCoord2xvOES :: MonadIO m => Ptr GLfixed -> m ()
glTexCoord2xvOES = ffiPtrfixedIOV glTexCoord2xvOESFunPtr

glTexCoord2xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glTexCoord2xvOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord2xvOES")

{-# NOINLINE glTexCoord2xvOESFunPtr #-}

-- | Usage: @'glTexCoord3xOES' s t r@


glTexCoord3xOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glTexCoord3xOES = ffifixedfixedfixedIOV glTexCoord3xOESFunPtr

glTexCoord3xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glTexCoord3xOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord3xOES")

{-# NOINLINE glTexCoord3xOESFunPtr #-}

-- | Usage: @'glTexCoord3xvOES' coords@
--
-- The length of @coords@ should be @3@.


glTexCoord3xvOES :: MonadIO m => Ptr GLfixed -> m ()
glTexCoord3xvOES = ffiPtrfixedIOV glTexCoord3xvOESFunPtr

glTexCoord3xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glTexCoord3xvOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord3xvOES")

{-# NOINLINE glTexCoord3xvOESFunPtr #-}

-- | Usage: @'glTexCoord4xOES' s t r q@


glTexCoord4xOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glTexCoord4xOES = ffifixedfixedfixedfixedIOV glTexCoord4xOESFunPtr

glTexCoord4xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glTexCoord4xOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord4xOES")

{-# NOINLINE glTexCoord4xOESFunPtr #-}

-- | Usage: @'glTexCoord4xvOES' coords@
--
-- The length of @coords@ should be @4@.


glTexCoord4xvOES :: MonadIO m => Ptr GLfixed -> m ()
glTexCoord4xvOES = ffiPtrfixedIOV glTexCoord4xvOESFunPtr

glTexCoord4xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glTexCoord4xvOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord4xvOES")

{-# NOINLINE glTexCoord4xvOESFunPtr #-}

-- | Usage: @'glTexEnvxOES' target pname param@


glTexEnvxOES :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glTexEnvxOES = ffienumenumfixedIOV glTexEnvxOESFunPtr

glTexEnvxOESFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glTexEnvxOESFunPtr = unsafePerformIO (getProcAddress "glTexEnvxOES")

{-# NOINLINE glTexEnvxOESFunPtr #-}

-- | Usage: @'glTexEnvxvOES' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTexEnvxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glTexEnvxvOES = ffienumenumPtrfixedIOV glTexEnvxvOESFunPtr

glTexEnvxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glTexEnvxvOESFunPtr = unsafePerformIO (getProcAddress "glTexEnvxvOES")

{-# NOINLINE glTexEnvxvOESFunPtr #-}

-- | Usage: @'glTexParameterxOES' target pname param@


glTexParameterxOES :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glTexParameterxOES = ffienumenumfixedIOV glTexParameterxOESFunPtr

glTexParameterxOESFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glTexParameterxOESFunPtr = unsafePerformIO (getProcAddress "glTexParameterxOES")

{-# NOINLINE glTexParameterxOESFunPtr #-}

-- | Usage: @'glTexParameterxvOES' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTexParameterxvOES :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glTexParameterxvOES = ffienumenumPtrfixedIOV glTexParameterxvOESFunPtr

glTexParameterxvOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glTexParameterxvOESFunPtr = unsafePerformIO (getProcAddress "glTexParameterxvOES")

{-# NOINLINE glTexParameterxvOESFunPtr #-}

-- | Usage: @'glTranslatexOES' x y z@


glTranslatexOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glTranslatexOES = ffifixedfixedfixedIOV glTranslatexOESFunPtr

glTranslatexOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glTranslatexOESFunPtr = unsafePerformIO (getProcAddress "glTranslatexOES")

{-# NOINLINE glTranslatexOESFunPtr #-}

-- | Usage: @'glVertex2xOES' x@


glVertex2xOES :: MonadIO m => GLfixed -> m ()
glVertex2xOES = ffifixedIOV glVertex2xOESFunPtr

glVertex2xOESFunPtr :: FunPtr (GLfixed -> IO ())
glVertex2xOESFunPtr = unsafePerformIO (getProcAddress "glVertex2xOES")

{-# NOINLINE glVertex2xOESFunPtr #-}

-- | Usage: @'glVertex2xvOES' coords@
--
-- The length of @coords@ should be @2@.


glVertex2xvOES :: MonadIO m => Ptr GLfixed -> m ()
glVertex2xvOES = ffiPtrfixedIOV glVertex2xvOESFunPtr

glVertex2xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glVertex2xvOESFunPtr = unsafePerformIO (getProcAddress "glVertex2xvOES")

{-# NOINLINE glVertex2xvOESFunPtr #-}

-- | Usage: @'glVertex3xOES' x y@


glVertex3xOES :: MonadIO m => GLfixed -> GLfixed -> m ()
glVertex3xOES = ffifixedfixedIOV glVertex3xOESFunPtr

glVertex3xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glVertex3xOESFunPtr = unsafePerformIO (getProcAddress "glVertex3xOES")

{-# NOINLINE glVertex3xOESFunPtr #-}

-- | Usage: @'glVertex3xvOES' coords@
--
-- The length of @coords@ should be @3@.


glVertex3xvOES :: MonadIO m => Ptr GLfixed -> m ()
glVertex3xvOES = ffiPtrfixedIOV glVertex3xvOESFunPtr

glVertex3xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glVertex3xvOESFunPtr = unsafePerformIO (getProcAddress "glVertex3xvOES")

{-# NOINLINE glVertex3xvOESFunPtr #-}

-- | Usage: @'glVertex4xOES' x y z@


glVertex4xOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glVertex4xOES = ffifixedfixedfixedIOV glVertex4xOESFunPtr

glVertex4xOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glVertex4xOESFunPtr = unsafePerformIO (getProcAddress "glVertex4xOES")

{-# NOINLINE glVertex4xOESFunPtr #-}

-- | Usage: @'glVertex4xvOES' coords@
--
-- The length of @coords@ should be @4@.


glVertex4xvOES :: MonadIO m => Ptr GLfixed -> m ()
glVertex4xvOES = ffiPtrfixedIOV glVertex4xvOESFunPtr

glVertex4xvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glVertex4xvOESFunPtr = unsafePerformIO (getProcAddress "glVertex4xvOES")

{-# NOINLINE glVertex4xvOESFunPtr #-}

pattern GL_FIXED_OES  :: (Eq a, Num a) => a

pattern GL_FIXED_OES = 0x140C