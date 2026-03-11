-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexBlend (
  -- * Extension Support
    gl_ARB_vertex_blend

  -- * GL_ARB_vertex_blend
  , glVertexBlendARB
  , glWeightPointerARB
  , glWeightbvARB
  , glWeightdvARB
  , glWeightfvARB
  , glWeightivARB
  , glWeightsvARB
  , glWeightubvARB
  , glWeightuivARB
  , glWeightusvARB
  , pattern GL_ACTIVE_VERTEX_UNITS_ARB
  , pattern GL_CURRENT_WEIGHT_ARB
  , pattern GL_MAX_VERTEX_UNITS_ARB
  , pattern GL_MODELVIEW0_ARB
  , pattern GL_MODELVIEW10_ARB
  , pattern GL_MODELVIEW11_ARB
  , pattern GL_MODELVIEW12_ARB
  , pattern GL_MODELVIEW13_ARB
  , pattern GL_MODELVIEW14_ARB
  , pattern GL_MODELVIEW15_ARB
  , pattern GL_MODELVIEW16_ARB
  , pattern GL_MODELVIEW17_ARB
  , pattern GL_MODELVIEW18_ARB
  , pattern GL_MODELVIEW19_ARB
  , pattern GL_MODELVIEW1_ARB
  , pattern GL_MODELVIEW20_ARB
  , pattern GL_MODELVIEW21_ARB
  , pattern GL_MODELVIEW22_ARB
  , pattern GL_MODELVIEW23_ARB
  , pattern GL_MODELVIEW24_ARB
  , pattern GL_MODELVIEW25_ARB
  , pattern GL_MODELVIEW26_ARB
  , pattern GL_MODELVIEW27_ARB
  , pattern GL_MODELVIEW28_ARB
  , pattern GL_MODELVIEW29_ARB
  , pattern GL_MODELVIEW2_ARB
  , pattern GL_MODELVIEW30_ARB
  , pattern GL_MODELVIEW31_ARB
  , pattern GL_MODELVIEW3_ARB
  , pattern GL_MODELVIEW4_ARB
  , pattern GL_MODELVIEW5_ARB
  , pattern GL_MODELVIEW6_ARB
  , pattern GL_MODELVIEW7_ARB
  , pattern GL_MODELVIEW8_ARB
  , pattern GL_MODELVIEW9_ARB
  , pattern GL_VERTEX_BLEND_ARB
  , pattern GL_WEIGHT_ARRAY_ARB
  , pattern GL_WEIGHT_ARRAY_POINTER_ARB
  , pattern GL_WEIGHT_ARRAY_SIZE_ARB
  , pattern GL_WEIGHT_ARRAY_STRIDE_ARB
  , pattern GL_WEIGHT_ARRAY_TYPE_ARB
  , pattern GL_WEIGHT_SUM_UNITY_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_blend.txt GL_ARB_vertex_blend> extension is available.

gl_ARB_vertex_blend :: Bool
gl_ARB_vertex_blend = member "GL_ARB_vertex_blend" extensions
{-# NOINLINE gl_ARB_vertex_blend #-}

-- | Usage: @'glVertexBlendARB' count@


glVertexBlendARB :: MonadIO m => GLint -> m ()
glVertexBlendARB = ffiintIOV glVertexBlendARBFunPtr

glVertexBlendARBFunPtr :: FunPtr (GLint -> IO ())
glVertexBlendARBFunPtr = unsafePerformIO (getProcAddress "glVertexBlendARB")

{-# NOINLINE glVertexBlendARBFunPtr #-}

-- | Usage: @'glWeightPointerARB' size type stride pointer@
--
-- The parameter @type@ is a @WeightPointerTypeARB@.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glWeightPointerARB :: MonadIO m => GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glWeightPointerARB = ffiintenumsizeiPtrVIOV glWeightPointerARBFunPtr

glWeightPointerARBFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glWeightPointerARBFunPtr = unsafePerformIO (getProcAddress "glWeightPointerARB")

{-# NOINLINE glWeightPointerARBFunPtr #-}

-- | Usage: @'glWeightbvARB' size weights@
--
-- The length of @weights@ should be @size@.


glWeightbvARB :: MonadIO m => GLint -> Ptr GLbyte -> m ()
glWeightbvARB = ffiintPtrbyteIOV glWeightbvARBFunPtr

glWeightbvARBFunPtr :: FunPtr (GLint -> Ptr GLbyte -> IO ())
glWeightbvARBFunPtr = unsafePerformIO (getProcAddress "glWeightbvARB")

{-# NOINLINE glWeightbvARBFunPtr #-}

-- | Usage: @'glWeightdvARB' size weights@
--
-- The length of @weights@ should be @size@.


glWeightdvARB :: MonadIO m => GLint -> Ptr GLdouble -> m ()
glWeightdvARB = ffiintPtrdoubleIOV glWeightdvARBFunPtr

glWeightdvARBFunPtr :: FunPtr (GLint -> Ptr GLdouble -> IO ())
glWeightdvARBFunPtr = unsafePerformIO (getProcAddress "glWeightdvARB")

{-# NOINLINE glWeightdvARBFunPtr #-}

-- | Usage: @'glWeightfvARB' size weights@
--
-- The length of @weights@ should be @size@.


glWeightfvARB :: MonadIO m => GLint -> Ptr GLfloat -> m ()
glWeightfvARB = ffiintPtrfloatIOV glWeightfvARBFunPtr

glWeightfvARBFunPtr :: FunPtr (GLint -> Ptr GLfloat -> IO ())
glWeightfvARBFunPtr = unsafePerformIO (getProcAddress "glWeightfvARB")

{-# NOINLINE glWeightfvARBFunPtr #-}

-- | Usage: @'glWeightivARB' size weights@
--
-- The length of @weights@ should be @size@.


glWeightivARB :: MonadIO m => GLint -> Ptr GLint -> m ()
glWeightivARB = ffiintPtrintIOV glWeightivARBFunPtr

glWeightivARBFunPtr :: FunPtr (GLint -> Ptr GLint -> IO ())
glWeightivARBFunPtr = unsafePerformIO (getProcAddress "glWeightivARB")

{-# NOINLINE glWeightivARBFunPtr #-}

-- | Usage: @'glWeightsvARB' size weights@
--
-- The length of @weights@ should be @size@.


glWeightsvARB :: MonadIO m => GLint -> Ptr GLshort -> m ()
glWeightsvARB = ffiintPtrshortIOV glWeightsvARBFunPtr

glWeightsvARBFunPtr :: FunPtr (GLint -> Ptr GLshort -> IO ())
glWeightsvARBFunPtr = unsafePerformIO (getProcAddress "glWeightsvARB")

{-# NOINLINE glWeightsvARBFunPtr #-}

-- | Usage: @'glWeightubvARB' size weights@
--
-- The length of @weights@ should be @size@.


glWeightubvARB :: MonadIO m => GLint -> Ptr GLubyte -> m ()
glWeightubvARB = ffiintPtrubyteIOV glWeightubvARBFunPtr

glWeightubvARBFunPtr :: FunPtr (GLint -> Ptr GLubyte -> IO ())
glWeightubvARBFunPtr = unsafePerformIO (getProcAddress "glWeightubvARB")

{-# NOINLINE glWeightubvARBFunPtr #-}

-- | Usage: @'glWeightuivARB' size weights@
--
-- The length of @weights@ should be @size@.


glWeightuivARB :: MonadIO m => GLint -> Ptr GLuint -> m ()
glWeightuivARB = ffiintPtruintIOV glWeightuivARBFunPtr

glWeightuivARBFunPtr :: FunPtr (GLint -> Ptr GLuint -> IO ())
glWeightuivARBFunPtr = unsafePerformIO (getProcAddress "glWeightuivARB")

{-# NOINLINE glWeightuivARBFunPtr #-}

-- | Usage: @'glWeightusvARB' size weights@
--
-- The length of @weights@ should be @size@.


glWeightusvARB :: MonadIO m => GLint -> Ptr GLushort -> m ()
glWeightusvARB = ffiintPtrushortIOV glWeightusvARBFunPtr

glWeightusvARBFunPtr :: FunPtr (GLint -> Ptr GLushort -> IO ())
glWeightusvARBFunPtr = unsafePerformIO (getProcAddress "glWeightusvARB")

{-# NOINLINE glWeightusvARBFunPtr #-}

pattern GL_ACTIVE_VERTEX_UNITS_ARB  :: (Eq a, Num a) => a

pattern GL_ACTIVE_VERTEX_UNITS_ARB = 0x86A5

pattern GL_CURRENT_WEIGHT_ARB  :: (Eq a, Num a) => a

pattern GL_CURRENT_WEIGHT_ARB = 0x86A8

pattern GL_MAX_VERTEX_UNITS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_UNITS_ARB = 0x86A4

pattern GL_MODELVIEW0_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW0_ARB = 0x1700

pattern GL_MODELVIEW10_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW10_ARB = 0x872A

pattern GL_MODELVIEW11_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW11_ARB = 0x872B

pattern GL_MODELVIEW12_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW12_ARB = 0x872C

pattern GL_MODELVIEW13_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW13_ARB = 0x872D

pattern GL_MODELVIEW14_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW14_ARB = 0x872E

pattern GL_MODELVIEW15_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW15_ARB = 0x872F

pattern GL_MODELVIEW16_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW16_ARB = 0x8730

pattern GL_MODELVIEW17_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW17_ARB = 0x8731

pattern GL_MODELVIEW18_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW18_ARB = 0x8732

pattern GL_MODELVIEW19_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW19_ARB = 0x8733

pattern GL_MODELVIEW1_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW1_ARB = 0x850A

pattern GL_MODELVIEW20_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW20_ARB = 0x8734

pattern GL_MODELVIEW21_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW21_ARB = 0x8735

pattern GL_MODELVIEW22_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW22_ARB = 0x8736

pattern GL_MODELVIEW23_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW23_ARB = 0x8737

pattern GL_MODELVIEW24_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW24_ARB = 0x8738

pattern GL_MODELVIEW25_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW25_ARB = 0x8739

pattern GL_MODELVIEW26_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW26_ARB = 0x873A

pattern GL_MODELVIEW27_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW27_ARB = 0x873B

pattern GL_MODELVIEW28_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW28_ARB = 0x873C

pattern GL_MODELVIEW29_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW29_ARB = 0x873D

pattern GL_MODELVIEW2_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW2_ARB = 0x8722

pattern GL_MODELVIEW30_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW30_ARB = 0x873E

pattern GL_MODELVIEW31_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW31_ARB = 0x873F

pattern GL_MODELVIEW3_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW3_ARB = 0x8723

pattern GL_MODELVIEW4_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW4_ARB = 0x8724

pattern GL_MODELVIEW5_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW5_ARB = 0x8725

pattern GL_MODELVIEW6_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW6_ARB = 0x8726

pattern GL_MODELVIEW7_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW7_ARB = 0x8727

pattern GL_MODELVIEW8_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW8_ARB = 0x8728

pattern GL_MODELVIEW9_ARB  :: (Eq a, Num a) => a

pattern GL_MODELVIEW9_ARB = 0x8729

pattern GL_VERTEX_BLEND_ARB  :: (Eq a, Num a) => a

pattern GL_VERTEX_BLEND_ARB = 0x86A7

pattern GL_WEIGHT_ARRAY_ARB  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_ARB = 0x86AD

pattern GL_WEIGHT_ARRAY_POINTER_ARB  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_POINTER_ARB = 0x86AC

pattern GL_WEIGHT_ARRAY_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_SIZE_ARB = 0x86AB

pattern GL_WEIGHT_ARRAY_STRIDE_ARB  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_STRIDE_ARB = 0x86AA

pattern GL_WEIGHT_ARRAY_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_TYPE_ARB = 0x86A9

pattern GL_WEIGHT_SUM_UNITY_ARB  :: (Eq a, Num a) => a

pattern GL_WEIGHT_SUM_UNITY_ARB = 0x86A6