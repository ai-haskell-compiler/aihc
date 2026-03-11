-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Framezoom (
  -- * Extension Support
    gl_SGIX_framezoom

  -- * GL_SGIX_framezoom
  , glFrameZoomSGIX
  , pattern GL_FRAMEZOOM_FACTOR_SGIX
  , pattern GL_FRAMEZOOM_SGIX
  , pattern GL_MAX_FRAMEZOOM_FACTOR_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/framezoom.txt GL_SGIX_framezoom> extension is available.

gl_SGIX_framezoom :: Bool
gl_SGIX_framezoom = member "GL_SGIX_framezoom" extensions
{-# NOINLINE gl_SGIX_framezoom #-}

-- | Usage: @'glFrameZoomSGIX' factor@
--
-- The parameter @factor@ is a @CheckedInt32@.


glFrameZoomSGIX :: MonadIO m => GLint -> m ()
glFrameZoomSGIX = ffiintIOV glFrameZoomSGIXFunPtr

glFrameZoomSGIXFunPtr :: FunPtr (GLint -> IO ())
glFrameZoomSGIXFunPtr = unsafePerformIO (getProcAddress "glFrameZoomSGIX")

{-# NOINLINE glFrameZoomSGIXFunPtr #-}

pattern GL_FRAMEZOOM_FACTOR_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAMEZOOM_FACTOR_SGIX = 0x818C

pattern GL_FRAMEZOOM_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAMEZOOM_SGIX = 0x818B

pattern GL_MAX_FRAMEZOOM_FACTOR_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_FRAMEZOOM_FACTOR_SGIX = 0x818D