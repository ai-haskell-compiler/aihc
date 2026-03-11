-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.ObjectPurgeable (
  -- * Extension Support
    gl_APPLE_object_purgeable

  -- * GL_APPLE_object_purgeable
  , glGetObjectParameterivAPPLE
  , glObjectPurgeableAPPLE
  , glObjectUnpurgeableAPPLE
  , pattern GL_BUFFER_OBJECT_APPLE
  , pattern GL_PURGEABLE_APPLE
  , pattern GL_RELEASED_APPLE
  , pattern GL_RETAINED_APPLE
  , pattern GL_UNDEFINED_APPLE
  , pattern GL_VOLATILE_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/object_purgeable.txt GL_APPLE_object_purgeable> extension is available.

gl_APPLE_object_purgeable :: Bool
gl_APPLE_object_purgeable = member "GL_APPLE_object_purgeable" extensions
{-# NOINLINE gl_APPLE_object_purgeable #-}

-- | Usage: @'glGetObjectParameterivAPPLE' objectType name pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetObjectParameterivAPPLE :: MonadIO m => GLenum -> GLuint -> GLenum -> Ptr GLint -> m ()
glGetObjectParameterivAPPLE = ffienumuintenumPtrintIOV glGetObjectParameterivAPPLEFunPtr

glGetObjectParameterivAPPLEFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
glGetObjectParameterivAPPLEFunPtr = unsafePerformIO (getProcAddress "glGetObjectParameterivAPPLE")

{-# NOINLINE glGetObjectParameterivAPPLEFunPtr #-}

-- | Usage: @'glObjectPurgeableAPPLE' objectType name option@


glObjectPurgeableAPPLE :: MonadIO m => GLenum -> GLuint -> GLenum -> m GLenum
glObjectPurgeableAPPLE = ffienumuintenumIOenum glObjectPurgeableAPPLEFunPtr

glObjectPurgeableAPPLEFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> IO GLenum)
glObjectPurgeableAPPLEFunPtr = unsafePerformIO (getProcAddress "glObjectPurgeableAPPLE")

{-# NOINLINE glObjectPurgeableAPPLEFunPtr #-}

-- | Usage: @'glObjectUnpurgeableAPPLE' objectType name option@


glObjectUnpurgeableAPPLE :: MonadIO m => GLenum -> GLuint -> GLenum -> m GLenum
glObjectUnpurgeableAPPLE = ffienumuintenumIOenum glObjectUnpurgeableAPPLEFunPtr

glObjectUnpurgeableAPPLEFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> IO GLenum)
glObjectUnpurgeableAPPLEFunPtr = unsafePerformIO (getProcAddress "glObjectUnpurgeableAPPLE")

{-# NOINLINE glObjectUnpurgeableAPPLEFunPtr #-}

pattern GL_BUFFER_OBJECT_APPLE  :: (Eq a, Num a) => a

pattern GL_BUFFER_OBJECT_APPLE = 0x85B3

pattern GL_PURGEABLE_APPLE  :: (Eq a, Num a) => a

pattern GL_PURGEABLE_APPLE = 0x8A1D

pattern GL_RELEASED_APPLE  :: (Eq a, Num a) => a

pattern GL_RELEASED_APPLE = 0x8A19

pattern GL_RETAINED_APPLE  :: (Eq a, Num a) => a

pattern GL_RETAINED_APPLE = 0x8A1B

pattern GL_UNDEFINED_APPLE  :: (Eq a, Num a) => a

pattern GL_UNDEFINED_APPLE = 0x8A1C

pattern GL_VOLATILE_APPLE  :: (Eq a, Num a) => a

pattern GL_VOLATILE_APPLE = 0x8A1A