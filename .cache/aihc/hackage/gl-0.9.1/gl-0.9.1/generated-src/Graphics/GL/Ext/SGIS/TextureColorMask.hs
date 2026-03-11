-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.TextureColorMask (
  -- * Extension Support
    gl_SGIS_texture_color_mask

  -- * GL_SGIS_texture_color_mask
  , glTextureColorMaskSGIS
  , pattern GL_TEXTURE_COLOR_WRITEMASK_SGIS
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/texture_color_mask.txt GL_SGIS_texture_color_mask> extension is available.

gl_SGIS_texture_color_mask :: Bool
gl_SGIS_texture_color_mask = member "GL_SGIS_texture_color_mask" extensions
{-# NOINLINE gl_SGIS_texture_color_mask #-}

-- | Usage: @'glTextureColorMaskSGIS' red green blue alpha@
--
-- The parameter @red@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @green@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @blue@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @alpha@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTextureColorMaskSGIS :: MonadIO m => GLboolean -> GLboolean -> GLboolean -> GLboolean -> m ()
glTextureColorMaskSGIS = ffibooleanbooleanbooleanbooleanIOV glTextureColorMaskSGISFunPtr

glTextureColorMaskSGISFunPtr :: FunPtr (GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
glTextureColorMaskSGISFunPtr = unsafePerformIO (getProcAddress "glTextureColorMaskSGIS")

{-# NOINLINE glTextureColorMaskSGISFunPtr #-}

pattern GL_TEXTURE_COLOR_WRITEMASK_SGIS  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COLOR_WRITEMASK_SGIS = 0x81EF