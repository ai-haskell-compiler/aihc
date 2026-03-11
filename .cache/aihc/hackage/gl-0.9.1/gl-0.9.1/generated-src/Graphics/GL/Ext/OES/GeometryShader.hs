-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.GeometryShader (
  -- * Extension Support
    gl_OES_geometry_shader

  -- * GL_OES_geometry_shader
  , glFramebufferTextureOES
  , pattern GL_FIRST_VERTEX_CONVENTION_OES
  , pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_OES
  , pattern GL_FRAMEBUFFER_DEFAULT_LAYERS_OES
  , pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_OES
  , pattern GL_GEOMETRY_LINKED_INPUT_TYPE_OES
  , pattern GL_GEOMETRY_LINKED_OUTPUT_TYPE_OES
  , pattern GL_GEOMETRY_LINKED_VERTICES_OUT_OES
  , pattern GL_GEOMETRY_SHADER_BIT_OES
  , pattern GL_GEOMETRY_SHADER_INVOCATIONS_OES
  , pattern GL_GEOMETRY_SHADER_OES
  , pattern GL_LAST_VERTEX_CONVENTION_OES
  , pattern GL_LAYER_PROVOKING_VERTEX_OES
  , pattern GL_LINES_ADJACENCY_OES
  , pattern GL_LINE_STRIP_ADJACENCY_OES
  , pattern GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS_OES
  , pattern GL_MAX_FRAMEBUFFER_LAYERS_OES
  , pattern GL_MAX_GEOMETRY_ATOMIC_COUNTERS_OES
  , pattern GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS_OES
  , pattern GL_MAX_GEOMETRY_IMAGE_UNIFORMS_OES
  , pattern GL_MAX_GEOMETRY_INPUT_COMPONENTS_OES
  , pattern GL_MAX_GEOMETRY_OUTPUT_COMPONENTS_OES
  , pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_OES
  , pattern GL_MAX_GEOMETRY_SHADER_INVOCATIONS_OES
  , pattern GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS_OES
  , pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_OES
  , pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_OES
  , pattern GL_MAX_GEOMETRY_UNIFORM_BLOCKS_OES
  , pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_OES
  , pattern GL_PRIMITIVES_GENERATED_OES
  , pattern GL_REFERENCED_BY_GEOMETRY_SHADER_OES
  , pattern GL_TRIANGLES_ADJACENCY_OES
  , pattern GL_TRIANGLE_STRIP_ADJACENCY_OES
  , pattern GL_UNDEFINED_VERTEX_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_geometry_shader.txt GL_OES_geometry_shader> extension is available.

gl_OES_geometry_shader :: Bool
gl_OES_geometry_shader = member "GL_OES_geometry_shader" extensions
{-# NOINLINE gl_OES_geometry_shader #-}

-- | Usage: @'glFramebufferTextureOES' target attachment texture level@
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


glFramebufferTextureOES :: MonadIO m => GLenum -> GLenum -> GLuint -> GLint -> m ()
glFramebufferTextureOES = ffienumenumuintintIOV glFramebufferTextureOESFunPtr

glFramebufferTextureOESFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
glFramebufferTextureOESFunPtr = unsafePerformIO (getProcAddress "glFramebufferTextureOES")

{-# NOINLINE glFramebufferTextureOESFunPtr #-}

pattern GL_FIRST_VERTEX_CONVENTION_OES  :: (Eq a, Num a) => a

pattern GL_FIRST_VERTEX_CONVENTION_OES = 0x8E4D

pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_OES = 0x8DA7

pattern GL_FRAMEBUFFER_DEFAULT_LAYERS_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_DEFAULT_LAYERS_OES = 0x9312

pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_OES = 0x8DA8

pattern GL_GEOMETRY_LINKED_INPUT_TYPE_OES  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_LINKED_INPUT_TYPE_OES = 0x8917

pattern GL_GEOMETRY_LINKED_OUTPUT_TYPE_OES  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_LINKED_OUTPUT_TYPE_OES = 0x8918

pattern GL_GEOMETRY_LINKED_VERTICES_OUT_OES  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_LINKED_VERTICES_OUT_OES = 0x8916

pattern GL_GEOMETRY_SHADER_BIT_OES  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_SHADER_BIT_OES = 0x00000004

pattern GL_GEOMETRY_SHADER_INVOCATIONS_OES  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_SHADER_INVOCATIONS_OES = 0x887F

pattern GL_GEOMETRY_SHADER_OES  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_SHADER_OES = 0x8DD9

pattern GL_LAST_VERTEX_CONVENTION_OES  :: (Eq a, Num a) => a

pattern GL_LAST_VERTEX_CONVENTION_OES = 0x8E4E

pattern GL_LAYER_PROVOKING_VERTEX_OES  :: (Eq a, Num a) => a

pattern GL_LAYER_PROVOKING_VERTEX_OES = 0x825E

pattern GL_LINES_ADJACENCY_OES  :: (Eq a, Num a) => a

pattern GL_LINES_ADJACENCY_OES = 0x000A

pattern GL_LINE_STRIP_ADJACENCY_OES  :: (Eq a, Num a) => a

pattern GL_LINE_STRIP_ADJACENCY_OES = 0x000B

pattern GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS_OES = 0x8A32

pattern GL_MAX_FRAMEBUFFER_LAYERS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_FRAMEBUFFER_LAYERS_OES = 0x9317

pattern GL_MAX_GEOMETRY_ATOMIC_COUNTERS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_ATOMIC_COUNTERS_OES = 0x92D5

pattern GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS_OES = 0x92CF

pattern GL_MAX_GEOMETRY_IMAGE_UNIFORMS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_IMAGE_UNIFORMS_OES = 0x90CD

pattern GL_MAX_GEOMETRY_INPUT_COMPONENTS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_INPUT_COMPONENTS_OES = 0x9123

pattern GL_MAX_GEOMETRY_OUTPUT_COMPONENTS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_OUTPUT_COMPONENTS_OES = 0x9124

pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_OES = 0x8DE0

pattern GL_MAX_GEOMETRY_SHADER_INVOCATIONS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_SHADER_INVOCATIONS_OES = 0x8E5A

pattern GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS_OES = 0x90D7

pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_OES = 0x8C29

pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_OES = 0x8DE1

pattern GL_MAX_GEOMETRY_UNIFORM_BLOCKS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_UNIFORM_BLOCKS_OES = 0x8A2C

pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_OES = 0x8DDF

pattern GL_PRIMITIVES_GENERATED_OES  :: (Eq a, Num a) => a

pattern GL_PRIMITIVES_GENERATED_OES = 0x8C87

pattern GL_REFERENCED_BY_GEOMETRY_SHADER_OES  :: (Eq a, Num a) => a

pattern GL_REFERENCED_BY_GEOMETRY_SHADER_OES = 0x9309

pattern GL_TRIANGLES_ADJACENCY_OES  :: (Eq a, Num a) => a

pattern GL_TRIANGLES_ADJACENCY_OES = 0x000C

pattern GL_TRIANGLE_STRIP_ADJACENCY_OES  :: (Eq a, Num a) => a

pattern GL_TRIANGLE_STRIP_ADJACENCY_OES = 0x000D

pattern GL_UNDEFINED_VERTEX_OES  :: (Eq a, Num a) => a

pattern GL_UNDEFINED_VERTEX_OES = 0x8260