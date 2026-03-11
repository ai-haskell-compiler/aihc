-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INTEL.FramebufferCMAA (
  -- * Extension Support
    gl_INTEL_framebuffer_CMAA

  -- * GL_INTEL_framebuffer_CMAA
  , glApplyFramebufferAttachmentCMAAINTEL
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/INTEL/framebuffer_CMAA.txt GL_INTEL_framebuffer_CMAA> extension is available.

gl_INTEL_framebuffer_CMAA :: Bool
gl_INTEL_framebuffer_CMAA = member "GL_INTEL_framebuffer_CMAA" extensions
{-# NOINLINE gl_INTEL_framebuffer_CMAA #-}

-- | Usage: @'glApplyFramebufferAttachmentCMAAINTEL'@


glApplyFramebufferAttachmentCMAAINTEL :: MonadIO m => m ()
glApplyFramebufferAttachmentCMAAINTEL = ffiIOV glApplyFramebufferAttachmentCMAAINTELFunPtr

glApplyFramebufferAttachmentCMAAINTELFunPtr :: FunPtr (IO ())
glApplyFramebufferAttachmentCMAAINTELFunPtr = unsafePerformIO (getProcAddress "glApplyFramebufferAttachmentCMAAINTEL")

{-# NOINLINE glApplyFramebufferAttachmentCMAAINTELFunPtr #-}