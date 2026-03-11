-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureObject (
  -- * Extension Support
    gl_EXT_texture_object

  -- * GL_EXT_texture_object
  , glAreTexturesResidentEXT
  , glBindTextureEXT
  , glDeleteTexturesEXT
  , glGenTexturesEXT
  , glIsTextureEXT
  , glPrioritizeTexturesEXT
  , pattern GL_TEXTURE_1D_BINDING_EXT
  , pattern GL_TEXTURE_2D_BINDING_EXT
  , pattern GL_TEXTURE_3D_BINDING_EXT
  , pattern GL_TEXTURE_PRIORITY_EXT
  , pattern GL_TEXTURE_RESIDENT_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_object.txt GL_EXT_texture_object> extension is available.

gl_EXT_texture_object :: Bool
gl_EXT_texture_object = member "GL_EXT_texture_object" extensions
{-# NOINLINE gl_EXT_texture_object #-}

-- | Usage: @'glAreTexturesResidentEXT' n textures residences@
--
-- The parameter @textures@ is a @Texture@.
--
-- The parameter @residences@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @textures@ should be @n@.
--
-- The length of @residences@ should be @n@.


glAreTexturesResidentEXT :: MonadIO m => GLsizei -> Ptr GLuint -> Ptr GLboolean -> m GLboolean
glAreTexturesResidentEXT = ffisizeiPtruintPtrbooleanIOboolean glAreTexturesResidentEXTFunPtr

glAreTexturesResidentEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean)
glAreTexturesResidentEXTFunPtr = unsafePerformIO (getProcAddress "glAreTexturesResidentEXT")

{-# NOINLINE glAreTexturesResidentEXTFunPtr #-}

-- | Usage: @'glBindTextureEXT' target texture@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @texture@ is a @Texture@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindTexture'.


glBindTextureEXT :: MonadIO m => GLenum -> GLuint -> m ()
glBindTextureEXT = ffienumuintIOV glBindTextureEXTFunPtr

glBindTextureEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBindTextureEXTFunPtr = unsafePerformIO (getProcAddress "glBindTextureEXT")

{-# NOINLINE glBindTextureEXTFunPtr #-}

-- | Usage: @'glDeleteTexturesEXT' n textures@
--
-- The parameter @textures@ is a @Texture@.
--
-- The length of @textures@ should be @n@.


glDeleteTexturesEXT :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteTexturesEXT = ffisizeiPtruintIOV glDeleteTexturesEXTFunPtr

glDeleteTexturesEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteTexturesEXTFunPtr = unsafePerformIO (getProcAddress "glDeleteTexturesEXT")

{-# NOINLINE glDeleteTexturesEXTFunPtr #-}

-- | Usage: @'glGenTexturesEXT' n textures@
--
-- The parameter @textures@ is a @Texture@.
--
-- The length of @textures@ should be @n@.


glGenTexturesEXT :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenTexturesEXT = ffisizeiPtruintIOV glGenTexturesEXTFunPtr

glGenTexturesEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenTexturesEXTFunPtr = unsafePerformIO (getProcAddress "glGenTexturesEXT")

{-# NOINLINE glGenTexturesEXTFunPtr #-}

-- | Usage: @'glIsTextureEXT' texture@
--
-- The parameter @texture@ is a @Texture@.


glIsTextureEXT :: MonadIO m => GLuint -> m GLboolean
glIsTextureEXT = ffiuintIOboolean glIsTextureEXTFunPtr

glIsTextureEXTFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsTextureEXTFunPtr = unsafePerformIO (getProcAddress "glIsTextureEXT")

{-# NOINLINE glIsTextureEXTFunPtr #-}

-- | Usage: @'glPrioritizeTexturesEXT' n textures priorities@
--
-- The parameter @textures@ is a @Texture@.
--
-- The parameter @priorities@ is a @ClampedFloat32@.
--
-- The length of @textures@ should be @n@.
--
-- The length of @priorities@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPrioritizeTextures'.


glPrioritizeTexturesEXT :: MonadIO m => GLsizei -> Ptr GLuint -> Ptr GLclampf -> m ()
glPrioritizeTexturesEXT = ffisizeiPtruintPtrclampfIOV glPrioritizeTexturesEXTFunPtr

glPrioritizeTexturesEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLclampf -> IO ())
glPrioritizeTexturesEXTFunPtr = unsafePerformIO (getProcAddress "glPrioritizeTexturesEXT")

{-# NOINLINE glPrioritizeTexturesEXTFunPtr #-}

pattern GL_TEXTURE_1D_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_1D_BINDING_EXT = 0x8068

pattern GL_TEXTURE_2D_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_2D_BINDING_EXT = 0x8069

pattern GL_TEXTURE_3D_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_3D_BINDING_EXT = 0x806A

pattern GL_TEXTURE_PRIORITY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_PRIORITY_EXT = 0x8066

pattern GL_TEXTURE_RESIDENT_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RESIDENT_EXT = 0x8067