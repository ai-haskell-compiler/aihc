-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.TextureRange (
  -- * Extension Support
    gl_APPLE_texture_range

  -- * GL_APPLE_texture_range
  , glGetTexParameterPointervAPPLE
  , glTextureRangeAPPLE
  , pattern GL_STORAGE_CACHED_APPLE
  , pattern GL_STORAGE_PRIVATE_APPLE
  , pattern GL_STORAGE_SHARED_APPLE
  , pattern GL_TEXTURE_RANGE_LENGTH_APPLE
  , pattern GL_TEXTURE_RANGE_POINTER_APPLE
  , pattern GL_TEXTURE_STORAGE_HINT_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/texture_range.txt GL_APPLE_texture_range> extension is available.

gl_APPLE_texture_range :: Bool
gl_APPLE_texture_range = member "GL_APPLE_texture_range" extensions
{-# NOINLINE gl_APPLE_texture_range #-}

-- | Usage: @'glGetTexParameterPointervAPPLE' target pname params@
--
-- The length of @params@ should be @1@.


glGetTexParameterPointervAPPLE :: MonadIO m => GLenum -> GLenum -> Ptr (Ptr ()) -> m ()
glGetTexParameterPointervAPPLE = ffienumenumPtrPtrVIOV glGetTexParameterPointervAPPLEFunPtr

glGetTexParameterPointervAPPLEFunPtr :: FunPtr (GLenum -> GLenum -> Ptr (Ptr ()) -> IO ())
glGetTexParameterPointervAPPLEFunPtr = unsafePerformIO (getProcAddress "glGetTexParameterPointervAPPLE")

{-# NOINLINE glGetTexParameterPointervAPPLEFunPtr #-}

-- | Usage: @'glTextureRangeAPPLE' target length pointer@
--
-- The length of @pointer@ should be @length@.


glTextureRangeAPPLE :: MonadIO m => GLenum -> GLsizei -> Ptr () -> m ()
glTextureRangeAPPLE = ffienumsizeiPtrVIOV glTextureRangeAPPLEFunPtr

glTextureRangeAPPLEFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr () -> IO ())
glTextureRangeAPPLEFunPtr = unsafePerformIO (getProcAddress "glTextureRangeAPPLE")

{-# NOINLINE glTextureRangeAPPLEFunPtr #-}

pattern GL_STORAGE_PRIVATE_APPLE  :: (Eq a, Num a) => a

pattern GL_STORAGE_PRIVATE_APPLE = 0x85BD

pattern GL_TEXTURE_RANGE_LENGTH_APPLE  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RANGE_LENGTH_APPLE = 0x85B7

pattern GL_TEXTURE_RANGE_POINTER_APPLE  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RANGE_POINTER_APPLE = 0x85B8

pattern GL_TEXTURE_STORAGE_HINT_APPLE  :: (Eq a, Num a) => a

pattern GL_TEXTURE_STORAGE_HINT_APPLE = 0x85BC