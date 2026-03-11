-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.Robustness (
  -- * Extension Support
    gl_ARB_robustness

  -- * GL_ARB_robustness
  , glGetGraphicsResetStatusARB
  , glGetnColorTableARB
  , glGetnCompressedTexImageARB
  , glGetnConvolutionFilterARB
  , glGetnHistogramARB
  , glGetnMapdvARB
  , glGetnMapfvARB
  , glGetnMapivARB
  , glGetnMinmaxARB
  , glGetnPixelMapfvARB
  , glGetnPixelMapuivARB
  , glGetnPixelMapusvARB
  , glGetnPolygonStippleARB
  , glGetnSeparableFilterARB
  , glGetnTexImageARB
  , glGetnUniformdvARB
  , glGetnUniformfvARB
  , glGetnUniformivARB
  , glGetnUniformuivARB
  , glReadnPixelsARB
  , pattern GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB
  , pattern GL_GUILTY_CONTEXT_RESET_ARB
  , pattern GL_INNOCENT_CONTEXT_RESET_ARB
  , pattern GL_LOSE_CONTEXT_ON_RESET_ARB
  , pattern GL_NO_ERROR
  , pattern GL_NO_RESET_NOTIFICATION_ARB
  , pattern GL_RESET_NOTIFICATION_STRATEGY_ARB
  , pattern GL_UNKNOWN_CONTEXT_RESET_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/robustness.txt GL_ARB_robustness> extension is available.

gl_ARB_robustness :: Bool
gl_ARB_robustness = member "GL_ARB_robustness" extensions
{-# NOINLINE gl_ARB_robustness #-}

-- | Usage: @'glGetGraphicsResetStatusARB'@


glGetGraphicsResetStatusARB :: MonadIO m => m GLenum
glGetGraphicsResetStatusARB = ffiIOenum glGetGraphicsResetStatusARBFunPtr

glGetGraphicsResetStatusARBFunPtr :: FunPtr (IO GLenum)
glGetGraphicsResetStatusARBFunPtr = unsafePerformIO (getProcAddress "glGetGraphicsResetStatusARB")

{-# NOINLINE glGetGraphicsResetStatusARBFunPtr #-}

-- | Usage: @'glGetnColorTableARB' target format type bufSize table@
--
-- The length of @table@ should be @bufSize@.


glGetnColorTableARB :: MonadIO m => GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnColorTableARB = ffienumenumenumsizeiPtrVIOV glGetnColorTableARBFunPtr

glGetnColorTableARBFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnColorTableARBFunPtr = unsafePerformIO (getProcAddress "glGetnColorTableARB")

{-# NOINLINE glGetnColorTableARBFunPtr #-}

-- | Usage: @'glGetnCompressedTexImageARB' target lod bufSize img@
--
-- The length of @img@ should be @bufSize@.


glGetnCompressedTexImageARB :: MonadIO m => GLenum -> GLint -> GLsizei -> Ptr () -> m ()
glGetnCompressedTexImageARB = ffienumintsizeiPtrVIOV glGetnCompressedTexImageARBFunPtr

glGetnCompressedTexImageARBFunPtr :: FunPtr (GLenum -> GLint -> GLsizei -> Ptr () -> IO ())
glGetnCompressedTexImageARBFunPtr = unsafePerformIO (getProcAddress "glGetnCompressedTexImageARB")

{-# NOINLINE glGetnCompressedTexImageARBFunPtr #-}

-- | Usage: @'glGetnConvolutionFilterARB' target format type bufSize image@
--
-- The length of @image@ should be @bufSize@.


glGetnConvolutionFilterARB :: MonadIO m => GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnConvolutionFilterARB = ffienumenumenumsizeiPtrVIOV glGetnConvolutionFilterARBFunPtr

glGetnConvolutionFilterARBFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnConvolutionFilterARBFunPtr = unsafePerformIO (getProcAddress "glGetnConvolutionFilterARB")

{-# NOINLINE glGetnConvolutionFilterARBFunPtr #-}

-- | Usage: @'glGetnHistogramARB' target reset format type bufSize values@
--
-- The parameter @reset@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @values@ should be @bufSize@.


glGetnHistogramARB :: MonadIO m => GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnHistogramARB = ffienumbooleanenumenumsizeiPtrVIOV glGetnHistogramARBFunPtr

glGetnHistogramARBFunPtr :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnHistogramARBFunPtr = unsafePerformIO (getProcAddress "glGetnHistogramARB")

{-# NOINLINE glGetnHistogramARBFunPtr #-}

-- | Usage: @'glGetnMapdvARB' target query bufSize v@
--
-- The length of @v@ should be @bufSize@.


glGetnMapdvARB :: MonadIO m => GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> m ()
glGetnMapdvARB = ffienumenumsizeiPtrdoubleIOV glGetnMapdvARBFunPtr

glGetnMapdvARBFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> IO ())
glGetnMapdvARBFunPtr = unsafePerformIO (getProcAddress "glGetnMapdvARB")

{-# NOINLINE glGetnMapdvARBFunPtr #-}

-- | Usage: @'glGetnMapfvARB' target query bufSize v@
--
-- The length of @v@ should be @bufSize@.


glGetnMapfvARB :: MonadIO m => GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> m ()
glGetnMapfvARB = ffienumenumsizeiPtrfloatIOV glGetnMapfvARBFunPtr

glGetnMapfvARBFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ())
glGetnMapfvARBFunPtr = unsafePerformIO (getProcAddress "glGetnMapfvARB")

{-# NOINLINE glGetnMapfvARBFunPtr #-}

-- | Usage: @'glGetnMapivARB' target query bufSize v@
--
-- The length of @v@ should be @bufSize@.


glGetnMapivARB :: MonadIO m => GLenum -> GLenum -> GLsizei -> Ptr GLint -> m ()
glGetnMapivARB = ffienumenumsizeiPtrintIOV glGetnMapivARBFunPtr

glGetnMapivARBFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ())
glGetnMapivARBFunPtr = unsafePerformIO (getProcAddress "glGetnMapivARB")

{-# NOINLINE glGetnMapivARBFunPtr #-}

-- | Usage: @'glGetnMinmaxARB' target reset format type bufSize values@
--
-- The parameter @reset@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @values@ should be @bufSize@.


glGetnMinmaxARB :: MonadIO m => GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnMinmaxARB = ffienumbooleanenumenumsizeiPtrVIOV glGetnMinmaxARBFunPtr

glGetnMinmaxARBFunPtr :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnMinmaxARBFunPtr = unsafePerformIO (getProcAddress "glGetnMinmaxARB")

{-# NOINLINE glGetnMinmaxARBFunPtr #-}

-- | Usage: @'glGetnPixelMapfvARB' map bufSize values@
--
-- The length of @values@ should be @bufSize@.


glGetnPixelMapfvARB :: MonadIO m => GLenum -> GLsizei -> Ptr GLfloat -> m ()
glGetnPixelMapfvARB = ffienumsizeiPtrfloatIOV glGetnPixelMapfvARBFunPtr

glGetnPixelMapfvARBFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
glGetnPixelMapfvARBFunPtr = unsafePerformIO (getProcAddress "glGetnPixelMapfvARB")

{-# NOINLINE glGetnPixelMapfvARBFunPtr #-}

-- | Usage: @'glGetnPixelMapuivARB' map bufSize values@
--
-- The length of @values@ should be @bufSize@.


glGetnPixelMapuivARB :: MonadIO m => GLenum -> GLsizei -> Ptr GLuint -> m ()
glGetnPixelMapuivARB = ffienumsizeiPtruintIOV glGetnPixelMapuivARBFunPtr

glGetnPixelMapuivARBFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
glGetnPixelMapuivARBFunPtr = unsafePerformIO (getProcAddress "glGetnPixelMapuivARB")

{-# NOINLINE glGetnPixelMapuivARBFunPtr #-}

-- | Usage: @'glGetnPixelMapusvARB' map bufSize values@
--
-- The length of @values@ should be @bufSize@.


glGetnPixelMapusvARB :: MonadIO m => GLenum -> GLsizei -> Ptr GLushort -> m ()
glGetnPixelMapusvARB = ffienumsizeiPtrushortIOV glGetnPixelMapusvARBFunPtr

glGetnPixelMapusvARBFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLushort -> IO ())
glGetnPixelMapusvARBFunPtr = unsafePerformIO (getProcAddress "glGetnPixelMapusvARB")

{-# NOINLINE glGetnPixelMapusvARBFunPtr #-}

-- | Usage: @'glGetnPolygonStippleARB' bufSize pattern@
--
-- The length of @pattern@ should be @bufSize@.


glGetnPolygonStippleARB :: MonadIO m => GLsizei -> Ptr GLubyte -> m ()
glGetnPolygonStippleARB = ffisizeiPtrubyteIOV glGetnPolygonStippleARBFunPtr

glGetnPolygonStippleARBFunPtr :: FunPtr (GLsizei -> Ptr GLubyte -> IO ())
glGetnPolygonStippleARBFunPtr = unsafePerformIO (getProcAddress "glGetnPolygonStippleARB")

{-# NOINLINE glGetnPolygonStippleARBFunPtr #-}

-- | Usage: @'glGetnSeparableFilterARB' target format type rowBufSize row columnBufSize column span@
--
-- The length of @row@ should be @rowBufSize@.
--
-- The length of @column@ should be @columnBufSize@.
--
-- The length of @span@ should be @0@.


glGetnSeparableFilterARB :: MonadIO m => GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> GLsizei -> Ptr () -> Ptr () -> m ()
glGetnSeparableFilterARB = ffienumenumenumsizeiPtrVsizeiPtrVPtrVIOV glGetnSeparableFilterARBFunPtr

glGetnSeparableFilterARBFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> GLsizei -> Ptr () -> Ptr () -> IO ())
glGetnSeparableFilterARBFunPtr = unsafePerformIO (getProcAddress "glGetnSeparableFilterARB")

{-# NOINLINE glGetnSeparableFilterARBFunPtr #-}

-- | Usage: @'glGetnTexImageARB' target level format type bufSize img@
--
-- The length of @img@ should be @bufSize@.


glGetnTexImageARB :: MonadIO m => GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnTexImageARB = ffienumintenumenumsizeiPtrVIOV glGetnTexImageARBFunPtr

glGetnTexImageARBFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnTexImageARBFunPtr = unsafePerformIO (getProcAddress "glGetnTexImageARB")

{-# NOINLINE glGetnTexImageARBFunPtr #-}

-- | Usage: @'glGetnUniformdvARB' program location bufSize params@
--
-- The length of @params@ should be @bufSize@.


glGetnUniformdvARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLdouble -> m ()
glGetnUniformdvARB = ffiuintintsizeiPtrdoubleIOV glGetnUniformdvARBFunPtr

glGetnUniformdvARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
glGetnUniformdvARBFunPtr = unsafePerformIO (getProcAddress "glGetnUniformdvARB")

{-# NOINLINE glGetnUniformdvARBFunPtr #-}

-- | Usage: @'glGetnUniformfvARB' program location bufSize params@
--
-- The length of @params@ should be @bufSize@.


glGetnUniformfvARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLfloat -> m ()
glGetnUniformfvARB = ffiuintintsizeiPtrfloatIOV glGetnUniformfvARBFunPtr

glGetnUniformfvARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
glGetnUniformfvARBFunPtr = unsafePerformIO (getProcAddress "glGetnUniformfvARB")

{-# NOINLINE glGetnUniformfvARBFunPtr #-}

-- | Usage: @'glGetnUniformivARB' program location bufSize params@
--
-- The length of @params@ should be @bufSize@.


glGetnUniformivARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLint -> m ()
glGetnUniformivARB = ffiuintintsizeiPtrintIOV glGetnUniformivARBFunPtr

glGetnUniformivARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
glGetnUniformivARBFunPtr = unsafePerformIO (getProcAddress "glGetnUniformivARB")

{-# NOINLINE glGetnUniformivARBFunPtr #-}

-- | Usage: @'glGetnUniformuivARB' program location bufSize params@
--
-- The length of @params@ should be @bufSize@.


glGetnUniformuivARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint -> m ()
glGetnUniformuivARB = ffiuintintsizeiPtruintIOV glGetnUniformuivARBFunPtr

glGetnUniformuivARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
glGetnUniformuivARBFunPtr = unsafePerformIO (getProcAddress "glGetnUniformuivARB")

{-# NOINLINE glGetnUniformuivARBFunPtr #-}

-- | Usage: @'glReadnPixelsARB' x y width height format type bufSize data@
--
-- The length of @data@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glReadnPixels'.


glReadnPixelsARB :: MonadIO m => GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glReadnPixelsARB = ffiintintsizeisizeienumenumsizeiPtrVIOV glReadnPixelsARBFunPtr

glReadnPixelsARBFunPtr :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glReadnPixelsARBFunPtr = unsafePerformIO (getProcAddress "glReadnPixelsARB")

{-# NOINLINE glReadnPixelsARBFunPtr #-}

pattern GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB  :: (Eq a, Num a) => a

pattern GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB = 0x00000004

pattern GL_GUILTY_CONTEXT_RESET_ARB  :: (Eq a, Num a) => a

pattern GL_GUILTY_CONTEXT_RESET_ARB = 0x8253

pattern GL_INNOCENT_CONTEXT_RESET_ARB  :: (Eq a, Num a) => a

pattern GL_INNOCENT_CONTEXT_RESET_ARB = 0x8254

pattern GL_LOSE_CONTEXT_ON_RESET_ARB  :: (Eq a, Num a) => a

pattern GL_LOSE_CONTEXT_ON_RESET_ARB = 0x8252

pattern GL_NO_RESET_NOTIFICATION_ARB  :: (Eq a, Num a) => a

pattern GL_NO_RESET_NOTIFICATION_ARB = 0x8261

pattern GL_RESET_NOTIFICATION_STRATEGY_ARB  :: (Eq a, Num a) => a

pattern GL_RESET_NOTIFICATION_STRATEGY_ARB = 0x8256

pattern GL_UNKNOWN_CONTEXT_RESET_ARB  :: (Eq a, Num a) => a

pattern GL_UNKNOWN_CONTEXT_RESET_ARB = 0x8255