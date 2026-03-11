-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SUN.TriangleList (
  -- * Extension Support
    gl_SUN_triangle_list

  -- * GL_SUN_triangle_list
  , glReplacementCodePointerSUN
  , glReplacementCodeubSUN
  , glReplacementCodeubvSUN
  , glReplacementCodeuiSUN
  , glReplacementCodeuivSUN
  , glReplacementCodeusSUN
  , glReplacementCodeusvSUN
  , pattern GL_R1UI_C3F_V3F_SUN
  , pattern GL_R1UI_C4F_N3F_V3F_SUN
  , pattern GL_R1UI_C4UB_V3F_SUN
  , pattern GL_R1UI_N3F_V3F_SUN
  , pattern GL_R1UI_T2F_C4F_N3F_V3F_SUN
  , pattern GL_R1UI_T2F_N3F_V3F_SUN
  , pattern GL_R1UI_T2F_V3F_SUN
  , pattern GL_R1UI_V3F_SUN
  , pattern GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN
  , pattern GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN
  , pattern GL_REPLACEMENT_CODE_ARRAY_SUN
  , pattern GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN
  , pattern GL_REPLACEMENT_CODE_SUN
  , pattern GL_REPLACE_MIDDLE_SUN
  , pattern GL_REPLACE_OLDEST_SUN
  , pattern GL_RESTART_SUN
  , pattern GL_TRIANGLE_LIST_SUN
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SUN/triangle_list.txt GL_SUN_triangle_list> extension is available.

gl_SUN_triangle_list :: Bool
gl_SUN_triangle_list = member "GL_SUN_triangle_list" extensions
{-# NOINLINE gl_SUN_triangle_list #-}

-- | Usage: @'glReplacementCodePointerSUN' type stride pointer@
--
-- The parameter @type@ is a @ReplacementCodeTypeSUN@.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glReplacementCodePointerSUN :: MonadIO m => GLenum -> GLsizei -> Ptr (Ptr ()) -> m ()
glReplacementCodePointerSUN = ffienumsizeiPtrPtrVIOV glReplacementCodePointerSUNFunPtr

glReplacementCodePointerSUNFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr ()) -> IO ())
glReplacementCodePointerSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodePointerSUN")

{-# NOINLINE glReplacementCodePointerSUNFunPtr #-}

-- | Usage: @'glReplacementCodeubSUN' code@


glReplacementCodeubSUN :: MonadIO m => GLubyte -> m ()
glReplacementCodeubSUN = ffiubyteIOV glReplacementCodeubSUNFunPtr

glReplacementCodeubSUNFunPtr :: FunPtr (GLubyte -> IO ())
glReplacementCodeubSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeubSUN")

{-# NOINLINE glReplacementCodeubSUNFunPtr #-}

-- | Usage: @'glReplacementCodeubvSUN' code@
--
-- The length of @code@ should be @COMPSIZE()@.


glReplacementCodeubvSUN :: MonadIO m => Ptr GLubyte -> m ()
glReplacementCodeubvSUN = ffiPtrubyteIOV glReplacementCodeubvSUNFunPtr

glReplacementCodeubvSUNFunPtr :: FunPtr (Ptr GLubyte -> IO ())
glReplacementCodeubvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeubvSUN")

{-# NOINLINE glReplacementCodeubvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiSUN' code@


glReplacementCodeuiSUN :: MonadIO m => GLuint -> m ()
glReplacementCodeuiSUN = ffiuintIOV glReplacementCodeuiSUNFunPtr

glReplacementCodeuiSUNFunPtr :: FunPtr (GLuint -> IO ())
glReplacementCodeuiSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiSUN")

{-# NOINLINE glReplacementCodeuiSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuivSUN' code@
--
-- The length of @code@ should be @COMPSIZE()@.


glReplacementCodeuivSUN :: MonadIO m => Ptr GLuint -> m ()
glReplacementCodeuivSUN = ffiPtruintIOV glReplacementCodeuivSUNFunPtr

glReplacementCodeuivSUNFunPtr :: FunPtr (Ptr GLuint -> IO ())
glReplacementCodeuivSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuivSUN")

{-# NOINLINE glReplacementCodeuivSUNFunPtr #-}

-- | Usage: @'glReplacementCodeusSUN' code@


glReplacementCodeusSUN :: MonadIO m => GLushort -> m ()
glReplacementCodeusSUN = ffiushortIOV glReplacementCodeusSUNFunPtr

glReplacementCodeusSUNFunPtr :: FunPtr (GLushort -> IO ())
glReplacementCodeusSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeusSUN")

{-# NOINLINE glReplacementCodeusSUNFunPtr #-}

-- | Usage: @'glReplacementCodeusvSUN' code@
--
-- The length of @code@ should be @COMPSIZE()@.


glReplacementCodeusvSUN :: MonadIO m => Ptr GLushort -> m ()
glReplacementCodeusvSUN = ffiPtrushortIOV glReplacementCodeusvSUNFunPtr

glReplacementCodeusvSUNFunPtr :: FunPtr (Ptr GLushort -> IO ())
glReplacementCodeusvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeusvSUN")

{-# NOINLINE glReplacementCodeusvSUNFunPtr #-}

pattern GL_R1UI_C3F_V3F_SUN  :: (Eq a, Num a) => a

pattern GL_R1UI_C3F_V3F_SUN = 0x85C6

pattern GL_R1UI_C4F_N3F_V3F_SUN  :: (Eq a, Num a) => a

pattern GL_R1UI_C4F_N3F_V3F_SUN = 0x85C8

pattern GL_R1UI_C4UB_V3F_SUN  :: (Eq a, Num a) => a

pattern GL_R1UI_C4UB_V3F_SUN = 0x85C5

pattern GL_R1UI_N3F_V3F_SUN  :: (Eq a, Num a) => a

pattern GL_R1UI_N3F_V3F_SUN = 0x85C7

pattern GL_R1UI_T2F_C4F_N3F_V3F_SUN  :: (Eq a, Num a) => a

pattern GL_R1UI_T2F_C4F_N3F_V3F_SUN = 0x85CB

pattern GL_R1UI_T2F_N3F_V3F_SUN  :: (Eq a, Num a) => a

pattern GL_R1UI_T2F_N3F_V3F_SUN = 0x85CA

pattern GL_R1UI_T2F_V3F_SUN  :: (Eq a, Num a) => a

pattern GL_R1UI_T2F_V3F_SUN = 0x85C9

pattern GL_R1UI_V3F_SUN  :: (Eq a, Num a) => a

pattern GL_R1UI_V3F_SUN = 0x85C4

pattern GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN  :: (Eq a, Num a) => a

pattern GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN = 0x85C3

pattern GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN  :: (Eq a, Num a) => a

pattern GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN = 0x85C2

pattern GL_REPLACEMENT_CODE_ARRAY_SUN  :: (Eq a, Num a) => a

pattern GL_REPLACEMENT_CODE_ARRAY_SUN = 0x85C0

pattern GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN  :: (Eq a, Num a) => a

pattern GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN = 0x85C1

pattern GL_REPLACEMENT_CODE_SUN  :: (Eq a, Num a) => a

pattern GL_REPLACEMENT_CODE_SUN = 0x81D8

pattern GL_REPLACE_MIDDLE_SUN  :: (Eq a, Num a) => a

pattern GL_REPLACE_MIDDLE_SUN = 0x0002

pattern GL_REPLACE_OLDEST_SUN  :: (Eq a, Num a) => a

pattern GL_REPLACE_OLDEST_SUN = 0x0003

pattern GL_RESTART_SUN  :: (Eq a, Num a) => a

pattern GL_RESTART_SUN = 0x0001

pattern GL_TRIANGLE_LIST_SUN  :: (Eq a, Num a) => a

pattern GL_TRIANGLE_LIST_SUN = 0x81D7