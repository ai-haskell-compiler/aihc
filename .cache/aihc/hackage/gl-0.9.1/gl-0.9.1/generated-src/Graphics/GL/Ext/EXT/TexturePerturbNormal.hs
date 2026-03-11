-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TexturePerturbNormal (
  -- * Extension Support
    gl_EXT_texture_perturb_normal

  -- * GL_EXT_texture_perturb_normal
  , glTextureNormalEXT
  , pattern GL_PERTURB_EXT
  , pattern GL_TEXTURE_NORMAL_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_perturb_normal.txt GL_EXT_texture_perturb_normal> extension is available.

gl_EXT_texture_perturb_normal :: Bool
gl_EXT_texture_perturb_normal = member "GL_EXT_texture_perturb_normal" extensions
{-# NOINLINE gl_EXT_texture_perturb_normal #-}

-- | Usage: @'glTextureNormalEXT' mode@
--
-- The parameter @mode@ is a @TextureNormalModeEXT@.


glTextureNormalEXT :: MonadIO m => GLenum -> m ()
glTextureNormalEXT = ffienumIOV glTextureNormalEXTFunPtr

glTextureNormalEXTFunPtr :: FunPtr (GLenum -> IO ())
glTextureNormalEXTFunPtr = unsafePerformIO (getProcAddress "glTextureNormalEXT")

{-# NOINLINE glTextureNormalEXTFunPtr #-}

pattern GL_PERTURB_EXT  :: (Eq a, Num a) => a

pattern GL_PERTURB_EXT = 0x85AE

pattern GL_TEXTURE_NORMAL_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_NORMAL_EXT = 0x85AF