-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.GeometryProgram4 (
  -- * Extension Support
    gl_NV_geometry_program4

  -- * GL_NV_geometry_program4
  , glFramebufferTextureEXT
  , glFramebufferTextureFaceEXT
  , glFramebufferTextureLayerEXT
  , glProgramVertexLimitNV
  , pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT
  , pattern GL_GEOMETRY_INPUT_TYPE_EXT
  , pattern GL_GEOMETRY_OUTPUT_TYPE_EXT
  , pattern GL_GEOMETRY_PROGRAM_NV
  , pattern GL_GEOMETRY_VERTICES_OUT_EXT
  , pattern GL_LINES_ADJACENCY_EXT
  , pattern GL_LINE_STRIP_ADJACENCY_EXT
  , pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT
  , pattern GL_MAX_PROGRAM_OUTPUT_VERTICES_NV
  , pattern GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV
  , pattern GL_PROGRAM_POINT_SIZE_EXT
  , pattern GL_TRIANGLES_ADJACENCY_EXT
  , pattern GL_TRIANGLE_STRIP_ADJACENCY_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/geometry_program4.txt GL_NV_geometry_program4> extension is available.

gl_NV_geometry_program4 :: Bool
gl_NV_geometry_program4 = member "GL_NV_geometry_program4" extensions
{-# NOINLINE gl_NV_geometry_program4 #-}

-- | Usage: @'glFramebufferTextureFaceEXT' target attachment texture level face@
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
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.GeometryShader4.glFramebufferTextureFaceARB'.


glFramebufferTextureFaceEXT :: MonadIO m => GLenum -> GLenum -> GLuint -> GLint -> GLenum -> m ()
glFramebufferTextureFaceEXT = ffienumenumuintintenumIOV glFramebufferTextureFaceEXTFunPtr

glFramebufferTextureFaceEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
glFramebufferTextureFaceEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferTextureFaceEXT")

{-# NOINLINE glFramebufferTextureFaceEXTFunPtr #-}

-- | Usage: @'glProgramVertexLimitNV' target limit@
--
-- The parameter @target@ is a @ProgramTarget@.


glProgramVertexLimitNV :: MonadIO m => GLenum -> GLint -> m ()
glProgramVertexLimitNV = ffienumintIOV glProgramVertexLimitNVFunPtr

glProgramVertexLimitNVFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glProgramVertexLimitNVFunPtr = unsafePerformIO (getProcAddress "glProgramVertexLimitNV")

{-# NOINLINE glProgramVertexLimitNVFunPtr #-}

pattern GL_GEOMETRY_PROGRAM_NV  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_PROGRAM_NV = 0x8C26

pattern GL_MAX_PROGRAM_OUTPUT_VERTICES_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_OUTPUT_VERTICES_NV = 0x8C27

pattern GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV = 0x8C28