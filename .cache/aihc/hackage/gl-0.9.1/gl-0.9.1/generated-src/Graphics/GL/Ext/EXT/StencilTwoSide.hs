-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.StencilTwoSide (
  -- * Extension Support
    gl_EXT_stencil_two_side

  -- * GL_EXT_stencil_two_side
  , glActiveStencilFaceEXT
  , pattern GL_ACTIVE_STENCIL_FACE_EXT
  , pattern GL_STENCIL_TEST_TWO_SIDE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/stencil_two_side.txt GL_EXT_stencil_two_side> extension is available.

gl_EXT_stencil_two_side :: Bool
gl_EXT_stencil_two_side = member "GL_EXT_stencil_two_side" extensions
{-# NOINLINE gl_EXT_stencil_two_side #-}

-- | Usage: @'glActiveStencilFaceEXT' face@
--
-- The parameter @face@ is a @StencilFaceDirection@.


glActiveStencilFaceEXT :: MonadIO m => GLenum -> m ()
glActiveStencilFaceEXT = ffienumIOV glActiveStencilFaceEXTFunPtr

glActiveStencilFaceEXTFunPtr :: FunPtr (GLenum -> IO ())
glActiveStencilFaceEXTFunPtr = unsafePerformIO (getProcAddress "glActiveStencilFaceEXT")

{-# NOINLINE glActiveStencilFaceEXTFunPtr #-}

pattern GL_ACTIVE_STENCIL_FACE_EXT  :: (Eq a, Num a) => a

pattern GL_ACTIVE_STENCIL_FACE_EXT = 0x8911

pattern GL_STENCIL_TEST_TWO_SIDE_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_TEST_TWO_SIDE_EXT = 0x8910