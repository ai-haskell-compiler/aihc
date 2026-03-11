-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.Evaluators (
  -- * Extension Support
    gl_NV_evaluators

  -- * GL_NV_evaluators
  , glEvalMapsNV
  , glGetMapAttribParameterfvNV
  , glGetMapAttribParameterivNV
  , glGetMapControlPointsNV
  , glGetMapParameterfvNV
  , glGetMapParameterivNV
  , glMapControlPointsNV
  , glMapParameterfvNV
  , glMapParameterivNV
  , pattern GL_EVAL_2D_NV
  , pattern GL_EVAL_FRACTIONAL_TESSELLATION_NV
  , pattern GL_EVAL_TRIANGULAR_2D_NV
  , pattern GL_EVAL_VERTEX_ATTRIB0_NV
  , pattern GL_EVAL_VERTEX_ATTRIB10_NV
  , pattern GL_EVAL_VERTEX_ATTRIB11_NV
  , pattern GL_EVAL_VERTEX_ATTRIB12_NV
  , pattern GL_EVAL_VERTEX_ATTRIB13_NV
  , pattern GL_EVAL_VERTEX_ATTRIB14_NV
  , pattern GL_EVAL_VERTEX_ATTRIB15_NV
  , pattern GL_EVAL_VERTEX_ATTRIB1_NV
  , pattern GL_EVAL_VERTEX_ATTRIB2_NV
  , pattern GL_EVAL_VERTEX_ATTRIB3_NV
  , pattern GL_EVAL_VERTEX_ATTRIB4_NV
  , pattern GL_EVAL_VERTEX_ATTRIB5_NV
  , pattern GL_EVAL_VERTEX_ATTRIB6_NV
  , pattern GL_EVAL_VERTEX_ATTRIB7_NV
  , pattern GL_EVAL_VERTEX_ATTRIB8_NV
  , pattern GL_EVAL_VERTEX_ATTRIB9_NV
  , pattern GL_MAP_ATTRIB_U_ORDER_NV
  , pattern GL_MAP_ATTRIB_V_ORDER_NV
  , pattern GL_MAP_TESSELLATION_NV
  , pattern GL_MAX_MAP_TESSELLATION_NV
  , pattern GL_MAX_RATIONAL_EVAL_ORDER_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/evaluators.txt GL_NV_evaluators> extension is available.

gl_NV_evaluators :: Bool
gl_NV_evaluators = member "GL_NV_evaluators" extensions
{-# NOINLINE gl_NV_evaluators #-}

-- | Usage: @'glEvalMapsNV' target mode@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @mode@ is a @EvalMapsModeNV@.


glEvalMapsNV :: MonadIO m => GLenum -> GLenum -> m ()
glEvalMapsNV = ffienumenumIOV glEvalMapsNVFunPtr

glEvalMapsNVFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glEvalMapsNVFunPtr = unsafePerformIO (getProcAddress "glEvalMapsNV")

{-# NOINLINE glEvalMapsNVFunPtr #-}

-- | Usage: @'glGetMapAttribParameterfvNV' target index pname params@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @pname@ is a @MapAttribParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMapAttribParameterfvNV :: MonadIO m => GLenum -> GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetMapAttribParameterfvNV = ffienumuintenumPtrfloatIOV glGetMapAttribParameterfvNVFunPtr

glGetMapAttribParameterfvNVFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetMapAttribParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetMapAttribParameterfvNV")

{-# NOINLINE glGetMapAttribParameterfvNVFunPtr #-}

-- | Usage: @'glGetMapAttribParameterivNV' target index pname params@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @pname@ is a @MapAttribParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMapAttribParameterivNV :: MonadIO m => GLenum -> GLuint -> GLenum -> Ptr GLint -> m ()
glGetMapAttribParameterivNV = ffienumuintenumPtrintIOV glGetMapAttribParameterivNVFunPtr

glGetMapAttribParameterivNVFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
glGetMapAttribParameterivNVFunPtr = unsafePerformIO (getProcAddress "glGetMapAttribParameterivNV")

{-# NOINLINE glGetMapAttribParameterivNVFunPtr #-}

-- | Usage: @'glGetMapControlPointsNV' target index type ustride vstride packed points@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @type@ is a @MapTypeNV@.
--
-- The parameter @packed@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @points@ should be @COMPSIZE(target)@.


glGetMapControlPointsNV :: MonadIO m => GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLboolean -> Ptr () -> m ()
glGetMapControlPointsNV = ffienumuintenumsizeisizeibooleanPtrVIOV glGetMapControlPointsNVFunPtr

glGetMapControlPointsNVFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLboolean -> Ptr () -> IO ())
glGetMapControlPointsNVFunPtr = unsafePerformIO (getProcAddress "glGetMapControlPointsNV")

{-# NOINLINE glGetMapControlPointsNVFunPtr #-}

-- | Usage: @'glGetMapParameterfvNV' target pname params@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @pname@ is a @MapParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(target,pname)@.


glGetMapParameterfvNV :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetMapParameterfvNV = ffienumenumPtrfloatIOV glGetMapParameterfvNVFunPtr

glGetMapParameterfvNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetMapParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetMapParameterfvNV")

{-# NOINLINE glGetMapParameterfvNVFunPtr #-}

-- | Usage: @'glGetMapParameterivNV' target pname params@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @pname@ is a @MapParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(target,pname)@.


glGetMapParameterivNV :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetMapParameterivNV = ffienumenumPtrintIOV glGetMapParameterivNVFunPtr

glGetMapParameterivNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetMapParameterivNVFunPtr = unsafePerformIO (getProcAddress "glGetMapParameterivNV")

{-# NOINLINE glGetMapParameterivNVFunPtr #-}

-- | Usage: @'glMapControlPointsNV' target index type ustride vstride uorder vorder packed points@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @type@ is a @MapTypeNV@.
--
-- The parameter @uorder@ is a @CheckedInt32@.
--
-- The parameter @vorder@ is a @CheckedInt32@.
--
-- The parameter @packed@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @points@ should be @COMPSIZE(target,uorder,vorder)@.


glMapControlPointsNV :: MonadIO m => GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLint -> GLboolean -> Ptr () -> m ()
glMapControlPointsNV = ffienumuintenumsizeisizeiintintbooleanPtrVIOV glMapControlPointsNVFunPtr

glMapControlPointsNVFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLint -> GLboolean -> Ptr () -> IO ())
glMapControlPointsNVFunPtr = unsafePerformIO (getProcAddress "glMapControlPointsNV")

{-# NOINLINE glMapControlPointsNVFunPtr #-}

-- | Usage: @'glMapParameterfvNV' target pname params@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @pname@ is a @MapParameterNV@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(target,pname)@.


glMapParameterfvNV :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glMapParameterfvNV = ffienumenumPtrfloatIOV glMapParameterfvNVFunPtr

glMapParameterfvNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glMapParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glMapParameterfvNV")

{-# NOINLINE glMapParameterfvNVFunPtr #-}

-- | Usage: @'glMapParameterivNV' target pname params@
--
-- The parameter @target@ is a @EvalTargetNV@.
--
-- The parameter @pname@ is a @MapParameterNV@.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(target,pname)@.


glMapParameterivNV :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glMapParameterivNV = ffienumenumPtrintIOV glMapParameterivNVFunPtr

glMapParameterivNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glMapParameterivNVFunPtr = unsafePerformIO (getProcAddress "glMapParameterivNV")

{-# NOINLINE glMapParameterivNVFunPtr #-}

pattern GL_EVAL_2D_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_2D_NV = 0x86C0

pattern GL_EVAL_FRACTIONAL_TESSELLATION_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_FRACTIONAL_TESSELLATION_NV = 0x86C5

pattern GL_EVAL_TRIANGULAR_2D_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_TRIANGULAR_2D_NV = 0x86C1

pattern GL_EVAL_VERTEX_ATTRIB0_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB0_NV = 0x86C6

pattern GL_EVAL_VERTEX_ATTRIB10_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB10_NV = 0x86D0

pattern GL_EVAL_VERTEX_ATTRIB11_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB11_NV = 0x86D1

pattern GL_EVAL_VERTEX_ATTRIB12_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB12_NV = 0x86D2

pattern GL_EVAL_VERTEX_ATTRIB13_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB13_NV = 0x86D3

pattern GL_EVAL_VERTEX_ATTRIB14_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB14_NV = 0x86D4

pattern GL_EVAL_VERTEX_ATTRIB15_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB15_NV = 0x86D5

pattern GL_EVAL_VERTEX_ATTRIB1_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB1_NV = 0x86C7

pattern GL_EVAL_VERTEX_ATTRIB2_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB2_NV = 0x86C8

pattern GL_EVAL_VERTEX_ATTRIB3_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB3_NV = 0x86C9

pattern GL_EVAL_VERTEX_ATTRIB4_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB4_NV = 0x86CA

pattern GL_EVAL_VERTEX_ATTRIB5_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB5_NV = 0x86CB

pattern GL_EVAL_VERTEX_ATTRIB6_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB6_NV = 0x86CC

pattern GL_EVAL_VERTEX_ATTRIB7_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB7_NV = 0x86CD

pattern GL_EVAL_VERTEX_ATTRIB8_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB8_NV = 0x86CE

pattern GL_EVAL_VERTEX_ATTRIB9_NV  :: (Eq a, Num a) => a

pattern GL_EVAL_VERTEX_ATTRIB9_NV = 0x86CF

pattern GL_MAP_ATTRIB_U_ORDER_NV  :: (Eq a, Num a) => a

pattern GL_MAP_ATTRIB_U_ORDER_NV = 0x86C3

pattern GL_MAP_ATTRIB_V_ORDER_NV  :: (Eq a, Num a) => a

pattern GL_MAP_ATTRIB_V_ORDER_NV = 0x86C4

pattern GL_MAP_TESSELLATION_NV  :: (Eq a, Num a) => a

pattern GL_MAP_TESSELLATION_NV = 0x86C2

pattern GL_MAX_MAP_TESSELLATION_NV  :: (Eq a, Num a) => a

pattern GL_MAX_MAP_TESSELLATION_NV = 0x86D6

pattern GL_MAX_RATIONAL_EVAL_ORDER_NV  :: (Eq a, Num a) => a

pattern GL_MAX_RATIONAL_EVAL_ORDER_NV = 0x86D7