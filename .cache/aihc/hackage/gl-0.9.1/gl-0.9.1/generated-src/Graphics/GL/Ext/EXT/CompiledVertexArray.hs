-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.CompiledVertexArray (
  -- * Extension Support
    gl_EXT_compiled_vertex_array

  -- * GL_EXT_compiled_vertex_array
  , glLockArraysEXT
  , glUnlockArraysEXT
  , pattern GL_ARRAY_ELEMENT_LOCK_COUNT_EXT
  , pattern GL_ARRAY_ELEMENT_LOCK_FIRST_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/compiled_vertex_array.txt GL_EXT_compiled_vertex_array> extension is available.

gl_EXT_compiled_vertex_array :: Bool
gl_EXT_compiled_vertex_array = member "GL_EXT_compiled_vertex_array" extensions
{-# NOINLINE gl_EXT_compiled_vertex_array #-}

-- | Usage: @'glLockArraysEXT' first count@


glLockArraysEXT :: MonadIO m => GLint -> GLsizei -> m ()
glLockArraysEXT = ffiintsizeiIOV glLockArraysEXTFunPtr

glLockArraysEXTFunPtr :: FunPtr (GLint -> GLsizei -> IO ())
glLockArraysEXTFunPtr = unsafePerformIO (getProcAddress "glLockArraysEXT")

{-# NOINLINE glLockArraysEXTFunPtr #-}

-- | Usage: @'glUnlockArraysEXT'@


glUnlockArraysEXT :: MonadIO m => m ()
glUnlockArraysEXT = ffiIOV glUnlockArraysEXTFunPtr

glUnlockArraysEXTFunPtr :: FunPtr (IO ())
glUnlockArraysEXTFunPtr = unsafePerformIO (getProcAddress "glUnlockArraysEXT")

{-# NOINLINE glUnlockArraysEXTFunPtr #-}

pattern GL_ARRAY_ELEMENT_LOCK_COUNT_EXT  :: (Eq a, Num a) => a

pattern GL_ARRAY_ELEMENT_LOCK_COUNT_EXT = 0x81A9

pattern GL_ARRAY_ELEMENT_LOCK_FIRST_EXT  :: (Eq a, Num a) => a

pattern GL_ARRAY_ELEMENT_LOCK_FIRST_EXT = 0x81A8