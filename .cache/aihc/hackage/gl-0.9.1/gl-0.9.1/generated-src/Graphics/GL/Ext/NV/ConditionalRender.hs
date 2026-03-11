-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ConditionalRender (
  -- * Extension Support
    gl_NV_conditional_render

  -- * GL_NV_conditional_render
  , glBeginConditionalRenderNV
  , glEndConditionalRenderNV
  , pattern GL_QUERY_BY_REGION_NO_WAIT_NV
  , pattern GL_QUERY_BY_REGION_WAIT_NV
  , pattern GL_QUERY_NO_WAIT_NV
  , pattern GL_QUERY_WAIT_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/conditional_render.txt GL_NV_conditional_render> extension is available.

gl_NV_conditional_render :: Bool
gl_NV_conditional_render = member "GL_NV_conditional_render" extensions
{-# NOINLINE gl_NV_conditional_render #-}

-- | Usage: @'glBeginConditionalRenderNV' id mode@
--
-- The parameter @mode@ is a @TypeEnum@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBeginConditionalRender'.


glBeginConditionalRenderNV :: MonadIO m => GLuint -> GLenum -> m ()
glBeginConditionalRenderNV = ffiuintenumIOV glBeginConditionalRenderNVFunPtr

glBeginConditionalRenderNVFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glBeginConditionalRenderNVFunPtr = unsafePerformIO (getProcAddress "glBeginConditionalRenderNV")

{-# NOINLINE glBeginConditionalRenderNVFunPtr #-}

-- | Usage: @'glEndConditionalRenderNV'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glEndConditionalRender'.


glEndConditionalRenderNV :: MonadIO m => m ()
glEndConditionalRenderNV = ffiIOV glEndConditionalRenderNVFunPtr

glEndConditionalRenderNVFunPtr :: FunPtr (IO ())
glEndConditionalRenderNVFunPtr = unsafePerformIO (getProcAddress "glEndConditionalRenderNV")

{-# NOINLINE glEndConditionalRenderNVFunPtr #-}

pattern GL_QUERY_BY_REGION_NO_WAIT_NV  :: (Eq a, Num a) => a

pattern GL_QUERY_BY_REGION_NO_WAIT_NV = 0x8E16

pattern GL_QUERY_BY_REGION_WAIT_NV  :: (Eq a, Num a) => a

pattern GL_QUERY_BY_REGION_WAIT_NV = 0x8E15

pattern GL_QUERY_NO_WAIT_NV  :: (Eq a, Num a) => a

pattern GL_QUERY_NO_WAIT_NV = 0x8E14

pattern GL_QUERY_WAIT_NV  :: (Eq a, Num a) => a

pattern GL_QUERY_WAIT_NV = 0x8E13