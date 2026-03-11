-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.VertexProgramEvaluators (
  -- * Extension Support
    gl_APPLE_vertex_program_evaluators

  -- * GL_APPLE_vertex_program_evaluators
  , glDisableVertexAttribAPPLE
  , glEnableVertexAttribAPPLE
  , glIsVertexAttribEnabledAPPLE
  , glMapVertexAttrib1dAPPLE
  , glMapVertexAttrib1fAPPLE
  , glMapVertexAttrib2dAPPLE
  , glMapVertexAttrib2fAPPLE
  , pattern GL_VERTEX_ATTRIB_MAP1_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP1_COEFF_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP1_DOMAIN_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP1_ORDER_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP1_SIZE_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP2_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP2_COEFF_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP2_DOMAIN_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP2_ORDER_APPLE
  , pattern GL_VERTEX_ATTRIB_MAP2_SIZE_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/vertex_program_evaluators.txt GL_APPLE_vertex_program_evaluators> extension is available.

gl_APPLE_vertex_program_evaluators :: Bool
gl_APPLE_vertex_program_evaluators = member "GL_APPLE_vertex_program_evaluators" extensions
{-# NOINLINE gl_APPLE_vertex_program_evaluators #-}

-- | Usage: @'glDisableVertexAttribAPPLE' index pname@


glDisableVertexAttribAPPLE :: MonadIO m => GLuint -> GLenum -> m ()
glDisableVertexAttribAPPLE = ffiuintenumIOV glDisableVertexAttribAPPLEFunPtr

glDisableVertexAttribAPPLEFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glDisableVertexAttribAPPLEFunPtr = unsafePerformIO (getProcAddress "glDisableVertexAttribAPPLE")

{-# NOINLINE glDisableVertexAttribAPPLEFunPtr #-}

-- | Usage: @'glEnableVertexAttribAPPLE' index pname@


glEnableVertexAttribAPPLE :: MonadIO m => GLuint -> GLenum -> m ()
glEnableVertexAttribAPPLE = ffiuintenumIOV glEnableVertexAttribAPPLEFunPtr

glEnableVertexAttribAPPLEFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glEnableVertexAttribAPPLEFunPtr = unsafePerformIO (getProcAddress "glEnableVertexAttribAPPLE")

{-# NOINLINE glEnableVertexAttribAPPLEFunPtr #-}

-- | Usage: @'glIsVertexAttribEnabledAPPLE' index pname@


glIsVertexAttribEnabledAPPLE :: MonadIO m => GLuint -> GLenum -> m GLboolean
glIsVertexAttribEnabledAPPLE = ffiuintenumIOboolean glIsVertexAttribEnabledAPPLEFunPtr

glIsVertexAttribEnabledAPPLEFunPtr :: FunPtr (GLuint -> GLenum -> IO GLboolean)
glIsVertexAttribEnabledAPPLEFunPtr = unsafePerformIO (getProcAddress "glIsVertexAttribEnabledAPPLE")

{-# NOINLINE glIsVertexAttribEnabledAPPLEFunPtr #-}

-- | Usage: @'glMapVertexAttrib1dAPPLE' index size u1 u2 stride order points@
--
-- The parameter @u1@ is a @CoordD@.
--
-- The parameter @u2@ is a @CoordD@.
--
-- The parameter @order@ is a @CheckedInt32@.
--
-- The parameter @points@ is a @CoordD@.
--
-- The length of @points@ should be @COMPSIZE(size,stride,order)@.


glMapVertexAttrib1dAPPLE :: MonadIO m => GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> m ()
glMapVertexAttrib1dAPPLE = ffiuintuintdoubledoubleintintPtrdoubleIOV glMapVertexAttrib1dAPPLEFunPtr

glMapVertexAttrib1dAPPLEFunPtr :: FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
glMapVertexAttrib1dAPPLEFunPtr = unsafePerformIO (getProcAddress "glMapVertexAttrib1dAPPLE")

{-# NOINLINE glMapVertexAttrib1dAPPLEFunPtr #-}

-- | Usage: @'glMapVertexAttrib1fAPPLE' index size u1 u2 stride order points@
--
-- The parameter @u1@ is a @CoordF@.
--
-- The parameter @u2@ is a @CoordF@.
--
-- The parameter @order@ is a @CheckedInt32@.
--
-- The parameter @points@ is a @CoordF@.
--
-- The length of @points@ should be @COMPSIZE(size,stride,order)@.


glMapVertexAttrib1fAPPLE :: MonadIO m => GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> m ()
glMapVertexAttrib1fAPPLE = ffiuintuintfloatfloatintintPtrfloatIOV glMapVertexAttrib1fAPPLEFunPtr

glMapVertexAttrib1fAPPLEFunPtr :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
glMapVertexAttrib1fAPPLEFunPtr = unsafePerformIO (getProcAddress "glMapVertexAttrib1fAPPLE")

{-# NOINLINE glMapVertexAttrib1fAPPLEFunPtr #-}

-- | Usage: @'glMapVertexAttrib2dAPPLE' index size u1 u2 ustride uorder v1 v2 vstride vorder points@
--
-- The parameter @u1@ is a @CoordD@.
--
-- The parameter @u2@ is a @CoordD@.
--
-- The parameter @uorder@ is a @CheckedInt32@.
--
-- The parameter @v1@ is a @CoordD@.
--
-- The parameter @v2@ is a @CoordD@.
--
-- The parameter @vorder@ is a @CheckedInt32@.
--
-- The parameter @points@ is a @CoordD@.
--
-- The length of @points@ should be @COMPSIZE(size,ustride,uorder,vstride,vorder)@.


glMapVertexAttrib2dAPPLE :: MonadIO m => GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> m ()
glMapVertexAttrib2dAPPLE = ffiuintuintdoubledoubleintintdoubledoubleintintPtrdoubleIOV glMapVertexAttrib2dAPPLEFunPtr

glMapVertexAttrib2dAPPLEFunPtr :: FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
glMapVertexAttrib2dAPPLEFunPtr = unsafePerformIO (getProcAddress "glMapVertexAttrib2dAPPLE")

{-# NOINLINE glMapVertexAttrib2dAPPLEFunPtr #-}

-- | Usage: @'glMapVertexAttrib2fAPPLE' index size u1 u2 ustride uorder v1 v2 vstride vorder points@
--
-- The parameter @u1@ is a @CoordF@.
--
-- The parameter @u2@ is a @CoordF@.
--
-- The parameter @uorder@ is a @CheckedInt32@.
--
-- The parameter @v1@ is a @CoordF@.
--
-- The parameter @v2@ is a @CoordF@.
--
-- The parameter @vorder@ is a @CheckedInt32@.
--
-- The parameter @points@ is a @CoordF@.
--
-- The length of @points@ should be @COMPSIZE(size,ustride,uorder,vstride,vorder)@.


glMapVertexAttrib2fAPPLE :: MonadIO m => GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> m ()
glMapVertexAttrib2fAPPLE = ffiuintuintfloatfloatintintfloatfloatintintPtrfloatIOV glMapVertexAttrib2fAPPLEFunPtr

glMapVertexAttrib2fAPPLEFunPtr :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
glMapVertexAttrib2fAPPLEFunPtr = unsafePerformIO (getProcAddress "glMapVertexAttrib2fAPPLE")

{-# NOINLINE glMapVertexAttrib2fAPPLEFunPtr #-}

pattern GL_VERTEX_ATTRIB_MAP1_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP1_APPLE = 0x8A00

pattern GL_VERTEX_ATTRIB_MAP1_COEFF_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP1_COEFF_APPLE = 0x8A03

pattern GL_VERTEX_ATTRIB_MAP1_DOMAIN_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP1_DOMAIN_APPLE = 0x8A05

pattern GL_VERTEX_ATTRIB_MAP1_ORDER_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP1_ORDER_APPLE = 0x8A04

pattern GL_VERTEX_ATTRIB_MAP1_SIZE_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP1_SIZE_APPLE = 0x8A02

pattern GL_VERTEX_ATTRIB_MAP2_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP2_APPLE = 0x8A01

pattern GL_VERTEX_ATTRIB_MAP2_COEFF_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP2_COEFF_APPLE = 0x8A07

pattern GL_VERTEX_ATTRIB_MAP2_DOMAIN_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP2_DOMAIN_APPLE = 0x8A09

pattern GL_VERTEX_ATTRIB_MAP2_ORDER_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP2_ORDER_APPLE = 0x8A08

pattern GL_VERTEX_ATTRIB_MAP2_SIZE_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_MAP2_SIZE_APPLE = 0x8A06