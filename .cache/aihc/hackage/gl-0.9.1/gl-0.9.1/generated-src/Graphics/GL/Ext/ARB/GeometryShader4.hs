-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.GeometryShader4 (
  -- * Extension Support
    gl_ARB_geometry_shader4

  -- * GL_ARB_geometry_shader4
  , glFramebufferTextureARB
  , glFramebufferTextureFaceARB
  , glFramebufferTextureLayerARB
  , glProgramParameteriARB
  , pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER
  , pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB
  , pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB
  , pattern GL_GEOMETRY_INPUT_TYPE_ARB
  , pattern GL_GEOMETRY_OUTPUT_TYPE_ARB
  , pattern GL_GEOMETRY_SHADER_ARB
  , pattern GL_GEOMETRY_VERTICES_OUT_ARB
  , pattern GL_LINES_ADJACENCY_ARB
  , pattern GL_LINE_STRIP_ADJACENCY_ARB
  , pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB
  , pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB
  , pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB
  , pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB
  , pattern GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB
  , pattern GL_MAX_VARYING_COMPONENTS
  , pattern GL_MAX_VERTEX_VARYING_COMPONENTS_ARB
  , pattern GL_PROGRAM_POINT_SIZE_ARB
  , pattern GL_TRIANGLES_ADJACENCY_ARB
  , pattern GL_TRIANGLE_STRIP_ADJACENCY_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/geometry_shader4.txt GL_ARB_geometry_shader4> extension is available.

gl_ARB_geometry_shader4 :: Bool
gl_ARB_geometry_shader4 = member "GL_ARB_geometry_shader4" extensions
{-# NOINLINE gl_ARB_geometry_shader4 #-}

-- | Usage: @'glFramebufferTextureARB' target attachment texture level@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFramebufferTexture'.


glFramebufferTextureARB :: MonadIO m => GLenum -> GLenum -> GLuint -> GLint -> m ()
glFramebufferTextureARB = ffienumenumuintintIOV glFramebufferTextureARBFunPtr

glFramebufferTextureARBFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
glFramebufferTextureARBFunPtr = unsafePerformIO (getProcAddress "glFramebufferTextureARB")

{-# NOINLINE glFramebufferTextureARBFunPtr #-}

-- | Usage: @'glFramebufferTextureFaceARB' target attachment texture level face@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @face@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.


glFramebufferTextureFaceARB :: MonadIO m => GLenum -> GLenum -> GLuint -> GLint -> GLenum -> m ()
glFramebufferTextureFaceARB = ffienumenumuintintenumIOV glFramebufferTextureFaceARBFunPtr

glFramebufferTextureFaceARBFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
glFramebufferTextureFaceARBFunPtr = unsafePerformIO (getProcAddress "glFramebufferTextureFaceARB")

{-# NOINLINE glFramebufferTextureFaceARBFunPtr #-}

-- | Usage: @'glFramebufferTextureLayerARB' target attachment texture level layer@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @layer@ is a @CheckedInt32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFramebufferTextureLayer'.


glFramebufferTextureLayerARB :: MonadIO m => GLenum -> GLenum -> GLuint -> GLint -> GLint -> m ()
glFramebufferTextureLayerARB = ffienumenumuintintintIOV glFramebufferTextureLayerARBFunPtr

glFramebufferTextureLayerARBFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
glFramebufferTextureLayerARBFunPtr = unsafePerformIO (getProcAddress "glFramebufferTextureLayerARB")

{-# NOINLINE glFramebufferTextureLayerARBFunPtr #-}

-- | Usage: @'glProgramParameteriARB' program pname value@
--
-- The parameter @pname@ is a @ProgramParameterPName@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glProgramParameteri'.


glProgramParameteriARB :: MonadIO m => GLuint -> GLenum -> GLint -> m ()
glProgramParameteriARB = ffiuintenumintIOV glProgramParameteriARBFunPtr

glProgramParameteriARBFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
glProgramParameteriARBFunPtr = unsafePerformIO (getProcAddress "glProgramParameteriARB")

{-# NOINLINE glProgramParameteriARBFunPtr #-}

pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB = 0x8DA7

pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB = 0x8DA9

pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB = 0x8DA8

pattern GL_GEOMETRY_INPUT_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_INPUT_TYPE_ARB = 0x8DDB

pattern GL_GEOMETRY_OUTPUT_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_OUTPUT_TYPE_ARB = 0x8DDC

pattern GL_GEOMETRY_SHADER_ARB  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_SHADER_ARB = 0x8DD9

pattern GL_GEOMETRY_VERTICES_OUT_ARB  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_VERTICES_OUT_ARB = 0x8DDA

pattern GL_LINES_ADJACENCY_ARB  :: (Eq a, Num a) => a

pattern GL_LINES_ADJACENCY_ARB = 0x000A

pattern GL_LINE_STRIP_ADJACENCY_ARB  :: (Eq a, Num a) => a

pattern GL_LINE_STRIP_ADJACENCY_ARB = 0x000B

pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB = 0x8DE0

pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB = 0x8C29

pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB = 0x8DE1

pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB = 0x8DDF

pattern GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB = 0x8DDD

pattern GL_MAX_VERTEX_VARYING_COMPONENTS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_VARYING_COMPONENTS_ARB = 0x8DDE

pattern GL_PROGRAM_POINT_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_PROGRAM_POINT_SIZE_ARB = 0x8642

pattern GL_TRIANGLES_ADJACENCY_ARB  :: (Eq a, Num a) => a

pattern GL_TRIANGLES_ADJACENCY_ARB = 0x000C

pattern GL_TRIANGLE_STRIP_ADJACENCY_ARB  :: (Eq a, Num a) => a

pattern GL_TRIANGLE_STRIP_ADJACENCY_ARB = 0x000D