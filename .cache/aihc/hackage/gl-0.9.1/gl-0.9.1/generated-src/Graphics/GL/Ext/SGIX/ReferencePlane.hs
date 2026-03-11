-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.ReferencePlane (
  -- * Extension Support
    gl_SGIX_reference_plane

  -- * GL_SGIX_reference_plane
  , glReferencePlaneSGIX
  , pattern GL_REFERENCE_PLANE_EQUATION_SGIX
  , pattern GL_REFERENCE_PLANE_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/reference_plane.txt GL_SGIX_reference_plane> extension is available.

gl_SGIX_reference_plane :: Bool
gl_SGIX_reference_plane = member "GL_SGIX_reference_plane" extensions
{-# NOINLINE gl_SGIX_reference_plane #-}

-- | Usage: @'glReferencePlaneSGIX' equation@
--
-- The length of @equation@ should be @4@.


glReferencePlaneSGIX :: MonadIO m => Ptr GLdouble -> m ()
glReferencePlaneSGIX = ffiPtrdoubleIOV glReferencePlaneSGIXFunPtr

glReferencePlaneSGIXFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glReferencePlaneSGIXFunPtr = unsafePerformIO (getProcAddress "glReferencePlaneSGIX")

{-# NOINLINE glReferencePlaneSGIXFunPtr #-}

pattern GL_REFERENCE_PLANE_EQUATION_SGIX  :: (Eq a, Num a) => a

pattern GL_REFERENCE_PLANE_EQUATION_SGIX = 0x817E

pattern GL_REFERENCE_PLANE_SGIX  :: (Eq a, Num a) => a

pattern GL_REFERENCE_PLANE_SGIX = 0x817D