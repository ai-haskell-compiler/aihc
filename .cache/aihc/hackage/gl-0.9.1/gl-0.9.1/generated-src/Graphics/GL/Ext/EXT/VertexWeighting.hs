-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.VertexWeighting (
  -- * Extension Support
    gl_EXT_vertex_weighting

  -- * GL_EXT_vertex_weighting
  , glVertexWeightPointerEXT
  , glVertexWeightfEXT
  , glVertexWeightfvEXT
  , pattern GL_CURRENT_VERTEX_WEIGHT_EXT
  , pattern GL_MODELVIEW0_EXT
  , pattern GL_MODELVIEW0_MATRIX_EXT
  , pattern GL_MODELVIEW0_STACK_DEPTH_EXT
  , pattern GL_MODELVIEW1_EXT
  , pattern GL_MODELVIEW1_MATRIX_EXT
  , pattern GL_MODELVIEW1_STACK_DEPTH_EXT
  , pattern GL_VERTEX_WEIGHTING_EXT
  , pattern GL_VERTEX_WEIGHT_ARRAY_EXT
  , pattern GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT
  , pattern GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT
  , pattern GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT
  , pattern GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/vertex_weighting.txt GL_EXT_vertex_weighting> extension is available.

gl_EXT_vertex_weighting :: Bool
gl_EXT_vertex_weighting = member "GL_EXT_vertex_weighting" extensions
{-# NOINLINE gl_EXT_vertex_weighting #-}

-- | Usage: @'glVertexWeightPointerEXT' size type stride pointer@
--
-- The parameter @type@ is a @VertexWeightPointerTypeEXT@.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glVertexWeightPointerEXT :: MonadIO m => GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glVertexWeightPointerEXT = ffiintenumsizeiPtrVIOV glVertexWeightPointerEXTFunPtr

glVertexWeightPointerEXTFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glVertexWeightPointerEXTFunPtr = unsafePerformIO (getProcAddress "glVertexWeightPointerEXT")

{-# NOINLINE glVertexWeightPointerEXTFunPtr #-}

-- | Usage: @'glVertexWeightfEXT' weight@
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.VertexWeighting.glVertexWeightfvEXT'.


glVertexWeightfEXT :: MonadIO m => GLfloat -> m ()
glVertexWeightfEXT = ffifloatIOV glVertexWeightfEXTFunPtr

glVertexWeightfEXTFunPtr :: FunPtr (GLfloat -> IO ())
glVertexWeightfEXTFunPtr = unsafePerformIO (getProcAddress "glVertexWeightfEXT")

{-# NOINLINE glVertexWeightfEXTFunPtr #-}

-- | Usage: @'glVertexWeightfvEXT' weight@
--
-- The length of @weight@ should be @1@.


glVertexWeightfvEXT :: MonadIO m => Ptr GLfloat -> m ()
glVertexWeightfvEXT = ffiPtrfloatIOV glVertexWeightfvEXTFunPtr

glVertexWeightfvEXTFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glVertexWeightfvEXTFunPtr = unsafePerformIO (getProcAddress "glVertexWeightfvEXT")

{-# NOINLINE glVertexWeightfvEXTFunPtr #-}

pattern GL_CURRENT_VERTEX_WEIGHT_EXT  :: (Eq a, Num a) => a

pattern GL_CURRENT_VERTEX_WEIGHT_EXT = 0x850B

pattern GL_MODELVIEW0_EXT  :: (Eq a, Num a) => a

pattern GL_MODELVIEW0_EXT = 0x1700

pattern GL_MODELVIEW0_MATRIX_EXT  :: (Eq a, Num a) => a

pattern GL_MODELVIEW0_MATRIX_EXT = 0x0BA6

pattern GL_MODELVIEW0_STACK_DEPTH_EXT  :: (Eq a, Num a) => a

pattern GL_MODELVIEW0_STACK_DEPTH_EXT = 0x0BA3

pattern GL_MODELVIEW1_EXT  :: (Eq a, Num a) => a

pattern GL_MODELVIEW1_EXT = 0x850A

pattern GL_MODELVIEW1_MATRIX_EXT  :: (Eq a, Num a) => a

pattern GL_MODELVIEW1_MATRIX_EXT = 0x8506

pattern GL_MODELVIEW1_STACK_DEPTH_EXT  :: (Eq a, Num a) => a

pattern GL_MODELVIEW1_STACK_DEPTH_EXT = 0x8502

pattern GL_VERTEX_WEIGHTING_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_WEIGHTING_EXT = 0x8509

pattern GL_VERTEX_WEIGHT_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_WEIGHT_ARRAY_EXT = 0x850C

pattern GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT = 0x8510

pattern GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT = 0x850D

pattern GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT = 0x850F

pattern GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT = 0x850E