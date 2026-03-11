-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.SinglePrecision (
  -- * Extension Support
    gl_OES_single_precision

  -- * GL_OES_single_precision
  , glClearDepthfOES
  , glClipPlanefOES
  , glDepthRangefOES
  , glFrustumfOES
  , glGetClipPlanefOES
  , glOrthofOES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_single_precision.txt GL_OES_single_precision> extension is available.

gl_OES_single_precision :: Bool
gl_OES_single_precision = member "GL_OES_single_precision" extensions
{-# NOINLINE gl_OES_single_precision #-}

-- | Usage: @'glClearDepthfOES' depth@
--
-- The parameter @depth@ is a @ClampedFloat32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glClearDepthf'.


glClearDepthfOES :: MonadIO m => GLclampf -> m ()
glClearDepthfOES = fficlampfIOV glClearDepthfOESFunPtr

glClearDepthfOESFunPtr :: FunPtr (GLclampf -> IO ())
glClearDepthfOESFunPtr = unsafePerformIO (getProcAddress "glClearDepthfOES")

{-# NOINLINE glClearDepthfOESFunPtr #-}

-- | Usage: @'glClipPlanefOES' plane equation@
--
-- The length of @equation@ should be @4@.


glClipPlanefOES :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glClipPlanefOES = ffienumPtrfloatIOV glClipPlanefOESFunPtr

glClipPlanefOESFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glClipPlanefOESFunPtr = unsafePerformIO (getProcAddress "glClipPlanefOES")

{-# NOINLINE glClipPlanefOESFunPtr #-}

-- | Usage: @'glDepthRangefOES' n f@
--
-- The parameter @n@ is a @ClampedFloat32@.
--
-- The parameter @f@ is a @ClampedFloat32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDepthRangef'.


glDepthRangefOES :: MonadIO m => GLclampf -> GLclampf -> m ()
glDepthRangefOES = fficlampfclampfIOV glDepthRangefOESFunPtr

glDepthRangefOESFunPtr :: FunPtr (GLclampf -> GLclampf -> IO ())
glDepthRangefOESFunPtr = unsafePerformIO (getProcAddress "glDepthRangefOES")

{-# NOINLINE glDepthRangefOESFunPtr #-}

-- | Usage: @'glFrustumfOES' l r b t n f@


glFrustumfOES :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glFrustumfOES = ffifloatfloatfloatfloatfloatfloatIOV glFrustumfOESFunPtr

glFrustumfOESFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glFrustumfOESFunPtr = unsafePerformIO (getProcAddress "glFrustumfOES")

{-# NOINLINE glFrustumfOESFunPtr #-}

-- | Usage: @'glGetClipPlanefOES' plane equation@
--
-- The length of @equation@ should be @4@.


glGetClipPlanefOES :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glGetClipPlanefOES = ffienumPtrfloatIOV glGetClipPlanefOESFunPtr

glGetClipPlanefOESFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glGetClipPlanefOESFunPtr = unsafePerformIO (getProcAddress "glGetClipPlanefOES")

{-# NOINLINE glGetClipPlanefOESFunPtr #-}

-- | Usage: @'glOrthofOES' l r b t n f@


glOrthofOES :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glOrthofOES = ffifloatfloatfloatfloatfloatfloatIOV glOrthofOESFunPtr

glOrthofOESFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glOrthofOESFunPtr = unsafePerformIO (getProcAddress "glOrthofOES")

{-# NOINLINE glOrthofOESFunPtr #-}