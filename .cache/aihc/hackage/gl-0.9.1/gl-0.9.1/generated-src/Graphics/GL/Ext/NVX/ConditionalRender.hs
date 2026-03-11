-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NVX.ConditionalRender (
  -- * Extension Support
    gl_NVX_conditional_render

  -- * GL_NVX_conditional_render
  , glBeginConditionalRenderNVX
  , glEndConditionalRenderNVX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_NVX_conditional_render extension is available.

gl_NVX_conditional_render :: Bool
gl_NVX_conditional_render = member "GL_NVX_conditional_render" extensions
{-# NOINLINE gl_NVX_conditional_render #-}

-- | Usage: @'glBeginConditionalRenderNVX' id@


glBeginConditionalRenderNVX :: MonadIO m => GLuint -> m ()
glBeginConditionalRenderNVX = ffiuintIOV glBeginConditionalRenderNVXFunPtr

glBeginConditionalRenderNVXFunPtr :: FunPtr (GLuint -> IO ())
glBeginConditionalRenderNVXFunPtr = unsafePerformIO (getProcAddress "glBeginConditionalRenderNVX")

{-# NOINLINE glBeginConditionalRenderNVXFunPtr #-}

-- | Usage: @'glEndConditionalRenderNVX'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glEndConditionalRender'.


glEndConditionalRenderNVX :: MonadIO m => m ()
glEndConditionalRenderNVX = ffiIOV glEndConditionalRenderNVXFunPtr

glEndConditionalRenderNVXFunPtr :: FunPtr (IO ())
glEndConditionalRenderNVXFunPtr = unsafePerformIO (getProcAddress "glEndConditionalRenderNVX")

{-# NOINLINE glEndConditionalRenderNVXFunPtr #-}