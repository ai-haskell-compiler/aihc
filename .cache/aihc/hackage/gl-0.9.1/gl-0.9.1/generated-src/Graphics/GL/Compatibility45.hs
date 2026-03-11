-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Compatibility45 (
  -- * Graphics.GL.Compatibility45
    module Graphics.GL.Compatibility44
  , module Graphics.GL.Core45
  , glGetnColorTable
  , glGetnConvolutionFilter
  , glGetnHistogram
  , glGetnMapdv
  , glGetnMapfv
  , glGetnMapiv
  , glGetnMinmax
  , glGetnPixelMapfv
  , glGetnPixelMapuiv
  , glGetnPixelMapusv
  , glGetnPolygonStipple
  , glGetnSeparableFilter
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

import Graphics.GL.Compatibility44
import Graphics.GL.Core45

-- | Usage: @'glGetnColorTable' target format type bufSize table@


glGetnColorTable :: MonadIO m => GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnColorTable = ffienumenumenumsizeiPtrVIOV glGetnColorTableFunPtr

glGetnColorTableFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnColorTableFunPtr = unsafePerformIO (getProcAddress "glGetnColorTable")

{-# NOINLINE glGetnColorTableFunPtr #-}

-- | Usage: @'glGetnConvolutionFilter' target format type bufSize image@


glGetnConvolutionFilter :: MonadIO m => GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnConvolutionFilter = ffienumenumenumsizeiPtrVIOV glGetnConvolutionFilterFunPtr

glGetnConvolutionFilterFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnConvolutionFilterFunPtr = unsafePerformIO (getProcAddress "glGetnConvolutionFilter")

{-# NOINLINE glGetnConvolutionFilterFunPtr #-}

-- | Usage: @'glGetnHistogram' target reset format type bufSize values@


glGetnHistogram :: MonadIO m => GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnHistogram = ffienumbooleanenumenumsizeiPtrVIOV glGetnHistogramFunPtr

glGetnHistogramFunPtr :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnHistogramFunPtr = unsafePerformIO (getProcAddress "glGetnHistogram")

{-# NOINLINE glGetnHistogramFunPtr #-}

-- | Usage: @'glGetnMapdv' target query bufSize v@


glGetnMapdv :: MonadIO m => GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> m ()
glGetnMapdv = ffienumenumsizeiPtrdoubleIOV glGetnMapdvFunPtr

glGetnMapdvFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> IO ())
glGetnMapdvFunPtr = unsafePerformIO (getProcAddress "glGetnMapdv")

{-# NOINLINE glGetnMapdvFunPtr #-}

-- | Usage: @'glGetnMapfv' target query bufSize v@


glGetnMapfv :: MonadIO m => GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> m ()
glGetnMapfv = ffienumenumsizeiPtrfloatIOV glGetnMapfvFunPtr

glGetnMapfvFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ())
glGetnMapfvFunPtr = unsafePerformIO (getProcAddress "glGetnMapfv")

{-# NOINLINE glGetnMapfvFunPtr #-}

-- | Usage: @'glGetnMapiv' target query bufSize v@


glGetnMapiv :: MonadIO m => GLenum -> GLenum -> GLsizei -> Ptr GLint -> m ()
glGetnMapiv = ffienumenumsizeiPtrintIOV glGetnMapivFunPtr

glGetnMapivFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ())
glGetnMapivFunPtr = unsafePerformIO (getProcAddress "glGetnMapiv")

{-# NOINLINE glGetnMapivFunPtr #-}

-- | Usage: @'glGetnMinmax' target reset format type bufSize values@


glGetnMinmax :: MonadIO m => GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnMinmax = ffienumbooleanenumenumsizeiPtrVIOV glGetnMinmaxFunPtr

glGetnMinmaxFunPtr :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnMinmaxFunPtr = unsafePerformIO (getProcAddress "glGetnMinmax")

{-# NOINLINE glGetnMinmaxFunPtr #-}

-- | Usage: @'glGetnPixelMapfv' map bufSize values@


glGetnPixelMapfv :: MonadIO m => GLenum -> GLsizei -> Ptr GLfloat -> m ()
glGetnPixelMapfv = ffienumsizeiPtrfloatIOV glGetnPixelMapfvFunPtr

glGetnPixelMapfvFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
glGetnPixelMapfvFunPtr = unsafePerformIO (getProcAddress "glGetnPixelMapfv")

{-# NOINLINE glGetnPixelMapfvFunPtr #-}

-- | Usage: @'glGetnPixelMapuiv' map bufSize values@


glGetnPixelMapuiv :: MonadIO m => GLenum -> GLsizei -> Ptr GLuint -> m ()
glGetnPixelMapuiv = ffienumsizeiPtruintIOV glGetnPixelMapuivFunPtr

glGetnPixelMapuivFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
glGetnPixelMapuivFunPtr = unsafePerformIO (getProcAddress "glGetnPixelMapuiv")

{-# NOINLINE glGetnPixelMapuivFunPtr #-}

-- | Usage: @'glGetnPixelMapusv' map bufSize values@


glGetnPixelMapusv :: MonadIO m => GLenum -> GLsizei -> Ptr GLushort -> m ()
glGetnPixelMapusv = ffienumsizeiPtrushortIOV glGetnPixelMapusvFunPtr

glGetnPixelMapusvFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLushort -> IO ())
glGetnPixelMapusvFunPtr = unsafePerformIO (getProcAddress "glGetnPixelMapusv")

{-# NOINLINE glGetnPixelMapusvFunPtr #-}

-- | Usage: @'glGetnPolygonStipple' bufSize pattern@


glGetnPolygonStipple :: MonadIO m => GLsizei -> Ptr GLubyte -> m ()
glGetnPolygonStipple = ffisizeiPtrubyteIOV glGetnPolygonStippleFunPtr

glGetnPolygonStippleFunPtr :: FunPtr (GLsizei -> Ptr GLubyte -> IO ())
glGetnPolygonStippleFunPtr = unsafePerformIO (getProcAddress "glGetnPolygonStipple")

{-# NOINLINE glGetnPolygonStippleFunPtr #-}

-- | Usage: @'glGetnSeparableFilter' target format type rowBufSize row columnBufSize column span@


glGetnSeparableFilter :: MonadIO m => GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> GLsizei -> Ptr () -> Ptr () -> m ()
glGetnSeparableFilter = ffienumenumenumsizeiPtrVsizeiPtrVPtrVIOV glGetnSeparableFilterFunPtr

glGetnSeparableFilterFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> GLsizei -> Ptr () -> Ptr () -> IO ())
glGetnSeparableFilterFunPtr = unsafePerformIO (getProcAddress "glGetnSeparableFilter")

{-# NOINLINE glGetnSeparableFilterFunPtr #-}