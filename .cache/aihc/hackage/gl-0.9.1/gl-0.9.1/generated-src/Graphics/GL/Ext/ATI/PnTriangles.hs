-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.PnTriangles (
  -- * Extension Support
    gl_ATI_pn_triangles

  -- * GL_ATI_pn_triangles
  , glPNTrianglesfATI
  , glPNTrianglesiATI
  , pattern GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI
  , pattern GL_PN_TRIANGLES_ATI
  , pattern GL_PN_TRIANGLES_NORMAL_MODE_ATI
  , pattern GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI
  , pattern GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI
  , pattern GL_PN_TRIANGLES_POINT_MODE_ATI
  , pattern GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI
  , pattern GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI
  , pattern GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/pn_triangles.txt GL_ATI_pn_triangles> extension is available.

gl_ATI_pn_triangles :: Bool
gl_ATI_pn_triangles = member "GL_ATI_pn_triangles" extensions
{-# NOINLINE gl_ATI_pn_triangles #-}

-- | Usage: @'glPNTrianglesfATI' pname param@
--
-- The parameter @pname@ is a @PNTrianglesPNameATI@.


glPNTrianglesfATI :: MonadIO m => GLenum -> GLfloat -> m ()
glPNTrianglesfATI = ffienumfloatIOV glPNTrianglesfATIFunPtr

glPNTrianglesfATIFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glPNTrianglesfATIFunPtr = unsafePerformIO (getProcAddress "glPNTrianglesfATI")

{-# NOINLINE glPNTrianglesfATIFunPtr #-}

-- | Usage: @'glPNTrianglesiATI' pname param@
--
-- The parameter @pname@ is a @PNTrianglesPNameATI@.


glPNTrianglesiATI :: MonadIO m => GLenum -> GLint -> m ()
glPNTrianglesiATI = ffienumintIOV glPNTrianglesiATIFunPtr

glPNTrianglesiATIFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glPNTrianglesiATIFunPtr = unsafePerformIO (getProcAddress "glPNTrianglesiATI")

{-# NOINLINE glPNTrianglesiATIFunPtr #-}

pattern GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI  :: (Eq a, Num a) => a

pattern GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI = 0x87F1

pattern GL_PN_TRIANGLES_ATI  :: (Eq a, Num a) => a

pattern GL_PN_TRIANGLES_ATI = 0x87F0

pattern GL_PN_TRIANGLES_NORMAL_MODE_ATI  :: (Eq a, Num a) => a

pattern GL_PN_TRIANGLES_NORMAL_MODE_ATI = 0x87F3

pattern GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI  :: (Eq a, Num a) => a

pattern GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI = 0x87F7

pattern GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI  :: (Eq a, Num a) => a

pattern GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI = 0x87F8

pattern GL_PN_TRIANGLES_POINT_MODE_ATI  :: (Eq a, Num a) => a

pattern GL_PN_TRIANGLES_POINT_MODE_ATI = 0x87F2

pattern GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI  :: (Eq a, Num a) => a

pattern GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI = 0x87F6

pattern GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI  :: (Eq a, Num a) => a

pattern GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI = 0x87F5

pattern GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI  :: (Eq a, Num a) => a

pattern GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI = 0x87F4