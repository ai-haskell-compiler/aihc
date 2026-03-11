-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.StencilClearTag (
  -- * Extension Support
    gl_EXT_stencil_clear_tag

  -- * GL_EXT_stencil_clear_tag
  , glStencilClearTagEXT
  , pattern GL_STENCIL_CLEAR_TAG_VALUE_EXT
  , pattern GL_STENCIL_TAG_BITS_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/stencil_clear_tag.txt GL_EXT_stencil_clear_tag> extension is available.

gl_EXT_stencil_clear_tag :: Bool
gl_EXT_stencil_clear_tag = member "GL_EXT_stencil_clear_tag" extensions
{-# NOINLINE gl_EXT_stencil_clear_tag #-}

-- | Usage: @'glStencilClearTagEXT' stencilTagBits stencilClearTag@


glStencilClearTagEXT :: MonadIO m => GLsizei -> GLuint -> m ()
glStencilClearTagEXT = ffisizeiuintIOV glStencilClearTagEXTFunPtr

glStencilClearTagEXTFunPtr :: FunPtr (GLsizei -> GLuint -> IO ())
glStencilClearTagEXTFunPtr = unsafePerformIO (getProcAddress "glStencilClearTagEXT")

{-# NOINLINE glStencilClearTagEXTFunPtr #-}

pattern GL_STENCIL_CLEAR_TAG_VALUE_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_CLEAR_TAG_VALUE_EXT = 0x88F3

pattern GL_STENCIL_TAG_BITS_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_TAG_BITS_EXT = 0x88F2