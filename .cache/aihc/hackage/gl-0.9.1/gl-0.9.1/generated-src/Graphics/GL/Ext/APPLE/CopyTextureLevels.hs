-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.CopyTextureLevels (
  -- * Extension Support
    gl_APPLE_copy_texture_levels

  -- * GL_APPLE_copy_texture_levels
  , glCopyTextureLevelsAPPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_copy_texture_levels.txt GL_APPLE_copy_texture_levels> extension is available.

gl_APPLE_copy_texture_levels :: Bool
gl_APPLE_copy_texture_levels = member "GL_APPLE_copy_texture_levels" extensions
{-# NOINLINE gl_APPLE_copy_texture_levels #-}

-- | Usage: @'glCopyTextureLevelsAPPLE' destinationTexture sourceTexture sourceBaseLevel sourceLevelCount@


glCopyTextureLevelsAPPLE :: MonadIO m => GLuint -> GLuint -> GLint -> GLsizei -> m ()
glCopyTextureLevelsAPPLE = ffiuintuintintsizeiIOV glCopyTextureLevelsAPPLEFunPtr

glCopyTextureLevelsAPPLEFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLsizei -> IO ())
glCopyTextureLevelsAPPLEFunPtr = unsafePerformIO (getProcAddress "glCopyTextureLevelsAPPLE")

{-# NOINLINE glCopyTextureLevelsAPPLEFunPtr #-}