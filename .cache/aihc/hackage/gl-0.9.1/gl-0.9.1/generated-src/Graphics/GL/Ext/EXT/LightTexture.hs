-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.LightTexture (
  -- * Extension Support
    gl_EXT_light_texture

  -- * GL_EXT_light_texture
  , glApplyTextureEXT
  , glTextureLightEXT
  , glTextureMaterialEXT
  , pattern GL_ATTENUATION_EXT
  , pattern GL_FRAGMENT_COLOR_EXT
  , pattern GL_FRAGMENT_DEPTH_EXT
  , pattern GL_FRAGMENT_MATERIAL_EXT
  , pattern GL_FRAGMENT_NORMAL_EXT
  , pattern GL_SHADOW_ATTENUATION_EXT
  , pattern GL_TEXTURE_APPLICATION_MODE_EXT
  , pattern GL_TEXTURE_LIGHT_EXT
  , pattern GL_TEXTURE_MATERIAL_FACE_EXT
  , pattern GL_TEXTURE_MATERIAL_PARAMETER_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/light_texture.txt GL_EXT_light_texture> extension is available.

gl_EXT_light_texture :: Bool
gl_EXT_light_texture = member "GL_EXT_light_texture" extensions
{-# NOINLINE gl_EXT_light_texture #-}

-- | Usage: @'glApplyTextureEXT' mode@
--
-- The parameter @mode@ is a @LightTextureModeEXT@.


glApplyTextureEXT :: MonadIO m => GLenum -> m ()
glApplyTextureEXT = ffienumIOV glApplyTextureEXTFunPtr

glApplyTextureEXTFunPtr :: FunPtr (GLenum -> IO ())
glApplyTextureEXTFunPtr = unsafePerformIO (getProcAddress "glApplyTextureEXT")

{-# NOINLINE glApplyTextureEXTFunPtr #-}

-- | Usage: @'glTextureLightEXT' pname@
--
-- The parameter @pname@ is a @LightTexturePNameEXT@.


glTextureLightEXT :: MonadIO m => GLenum -> m ()
glTextureLightEXT = ffienumIOV glTextureLightEXTFunPtr

glTextureLightEXTFunPtr :: FunPtr (GLenum -> IO ())
glTextureLightEXTFunPtr = unsafePerformIO (getProcAddress "glTextureLightEXT")

{-# NOINLINE glTextureLightEXTFunPtr #-}

-- | Usage: @'glTextureMaterialEXT' face mode@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @mode@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.


glTextureMaterialEXT :: MonadIO m => GLenum -> GLenum -> m ()
glTextureMaterialEXT = ffienumenumIOV glTextureMaterialEXTFunPtr

glTextureMaterialEXTFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glTextureMaterialEXTFunPtr = unsafePerformIO (getProcAddress "glTextureMaterialEXT")

{-# NOINLINE glTextureMaterialEXTFunPtr #-}

pattern GL_ATTENUATION_EXT  :: (Eq a, Num a) => a

pattern GL_ATTENUATION_EXT = 0x834D

pattern GL_FRAGMENT_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_COLOR_EXT = 0x834C

pattern GL_FRAGMENT_MATERIAL_EXT  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_MATERIAL_EXT = 0x8349

pattern GL_FRAGMENT_NORMAL_EXT  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_NORMAL_EXT = 0x834A

pattern GL_SHADOW_ATTENUATION_EXT  :: (Eq a, Num a) => a

pattern GL_SHADOW_ATTENUATION_EXT = 0x834E

pattern GL_TEXTURE_APPLICATION_MODE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_APPLICATION_MODE_EXT = 0x834F

pattern GL_TEXTURE_LIGHT_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LIGHT_EXT = 0x8350

pattern GL_TEXTURE_MATERIAL_FACE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MATERIAL_FACE_EXT = 0x8351

pattern GL_TEXTURE_MATERIAL_PARAMETER_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MATERIAL_PARAMETER_EXT = 0x8352