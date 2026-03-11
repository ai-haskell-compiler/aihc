-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderImageLoadStore (
  -- * Extension Support
    gl_EXT_shader_image_load_store

  -- * GL_EXT_shader_image_load_store
  , glBindImageTextureEXT
  , glMemoryBarrierEXT
  , pattern GL_ALL_BARRIER_BITS_EXT
  , pattern GL_ATOMIC_COUNTER_BARRIER_BIT_EXT
  , pattern GL_BUFFER_UPDATE_BARRIER_BIT_EXT
  , pattern GL_COMMAND_BARRIER_BIT_EXT
  , pattern GL_ELEMENT_ARRAY_BARRIER_BIT_EXT
  , pattern GL_FRAMEBUFFER_BARRIER_BIT_EXT
  , pattern GL_IMAGE_1D_ARRAY_EXT
  , pattern GL_IMAGE_1D_EXT
  , pattern GL_IMAGE_2D_ARRAY_EXT
  , pattern GL_IMAGE_2D_EXT
  , pattern GL_IMAGE_2D_MULTISAMPLE_ARRAY_EXT
  , pattern GL_IMAGE_2D_MULTISAMPLE_EXT
  , pattern GL_IMAGE_2D_RECT_EXT
  , pattern GL_IMAGE_3D_EXT
  , pattern GL_IMAGE_BINDING_ACCESS_EXT
  , pattern GL_IMAGE_BINDING_FORMAT_EXT
  , pattern GL_IMAGE_BINDING_LAYERED_EXT
  , pattern GL_IMAGE_BINDING_LAYER_EXT
  , pattern GL_IMAGE_BINDING_LEVEL_EXT
  , pattern GL_IMAGE_BINDING_NAME_EXT
  , pattern GL_IMAGE_BUFFER_EXT
  , pattern GL_IMAGE_CUBE_EXT
  , pattern GL_IMAGE_CUBE_MAP_ARRAY_EXT
  , pattern GL_INT_IMAGE_1D_ARRAY_EXT
  , pattern GL_INT_IMAGE_1D_EXT
  , pattern GL_INT_IMAGE_2D_ARRAY_EXT
  , pattern GL_INT_IMAGE_2D_EXT
  , pattern GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY_EXT
  , pattern GL_INT_IMAGE_2D_MULTISAMPLE_EXT
  , pattern GL_INT_IMAGE_2D_RECT_EXT
  , pattern GL_INT_IMAGE_3D_EXT
  , pattern GL_INT_IMAGE_BUFFER_EXT
  , pattern GL_INT_IMAGE_CUBE_EXT
  , pattern GL_INT_IMAGE_CUBE_MAP_ARRAY_EXT
  , pattern GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS_EXT
  , pattern GL_MAX_IMAGE_SAMPLES_EXT
  , pattern GL_MAX_IMAGE_UNITS_EXT
  , pattern GL_PIXEL_BUFFER_BARRIER_BIT_EXT
  , pattern GL_SHADER_IMAGE_ACCESS_BARRIER_BIT_EXT
  , pattern GL_TEXTURE_FETCH_BARRIER_BIT_EXT
  , pattern GL_TEXTURE_UPDATE_BARRIER_BIT_EXT
  , pattern GL_TRANSFORM_FEEDBACK_BARRIER_BIT_EXT
  , pattern GL_UNIFORM_BARRIER_BIT_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_1D_ARRAY_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_1D_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_2D_ARRAY_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_2D_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_2D_RECT_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_3D_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_BUFFER_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_CUBE_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY_EXT
  , pattern GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/shader_image_load_store.txt GL_EXT_shader_image_load_store> extension is available.

gl_EXT_shader_image_load_store :: Bool
gl_EXT_shader_image_load_store = member "GL_EXT_shader_image_load_store" extensions
{-# NOINLINE gl_EXT_shader_image_load_store #-}

-- | Usage: @'glBindImageTextureEXT' index texture level layered layer access format@
--
-- The parameter @layered@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glBindImageTextureEXT :: MonadIO m => GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLint -> m ()
glBindImageTextureEXT = ffiuintuintintbooleanintenumintIOV glBindImageTextureEXTFunPtr

glBindImageTextureEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLint -> IO ())
glBindImageTextureEXTFunPtr = unsafePerformIO (getProcAddress "glBindImageTextureEXT")

{-# NOINLINE glBindImageTextureEXTFunPtr #-}

-- | Usage: @'glMemoryBarrierEXT' barriers@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMemoryBarrier'.


glMemoryBarrierEXT :: MonadIO m => GLbitfield -> m ()
glMemoryBarrierEXT = ffibitfieldIOV glMemoryBarrierEXTFunPtr

glMemoryBarrierEXTFunPtr :: FunPtr (GLbitfield -> IO ())
glMemoryBarrierEXTFunPtr = unsafePerformIO (getProcAddress "glMemoryBarrierEXT")

{-# NOINLINE glMemoryBarrierEXTFunPtr #-}

pattern GL_ALL_BARRIER_BITS_EXT  :: (Eq a, Num a) => a

pattern GL_ALL_BARRIER_BITS_EXT = 0xFFFFFFFF

pattern GL_ATOMIC_COUNTER_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_ATOMIC_COUNTER_BARRIER_BIT_EXT = 0x00001000

pattern GL_BUFFER_UPDATE_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_BUFFER_UPDATE_BARRIER_BIT_EXT = 0x00000200

pattern GL_COMMAND_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_COMMAND_BARRIER_BIT_EXT = 0x00000040

pattern GL_ELEMENT_ARRAY_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_BARRIER_BIT_EXT = 0x00000002

pattern GL_FRAMEBUFFER_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_BARRIER_BIT_EXT = 0x00000400

pattern GL_IMAGE_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_1D_ARRAY_EXT = 0x9052

pattern GL_IMAGE_1D_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_1D_EXT = 0x904C

pattern GL_IMAGE_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_2D_ARRAY_EXT = 0x9053

pattern GL_IMAGE_2D_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_2D_EXT = 0x904D

pattern GL_IMAGE_2D_MULTISAMPLE_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_2D_MULTISAMPLE_ARRAY_EXT = 0x9056

pattern GL_IMAGE_2D_MULTISAMPLE_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_2D_MULTISAMPLE_EXT = 0x9055

pattern GL_IMAGE_2D_RECT_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_2D_RECT_EXT = 0x904F

pattern GL_IMAGE_3D_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_3D_EXT = 0x904E

pattern GL_IMAGE_BINDING_ACCESS_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_BINDING_ACCESS_EXT = 0x8F3E

pattern GL_IMAGE_BINDING_FORMAT_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_BINDING_FORMAT_EXT = 0x906E

pattern GL_IMAGE_BINDING_LAYERED_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_BINDING_LAYERED_EXT = 0x8F3C

pattern GL_IMAGE_BINDING_LAYER_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_BINDING_LAYER_EXT = 0x8F3D

pattern GL_IMAGE_BINDING_LEVEL_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_BINDING_LEVEL_EXT = 0x8F3B

pattern GL_IMAGE_BINDING_NAME_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_BINDING_NAME_EXT = 0x8F3A

pattern GL_IMAGE_CUBE_EXT  :: (Eq a, Num a) => a

pattern GL_IMAGE_CUBE_EXT = 0x9050

pattern GL_INT_IMAGE_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_1D_ARRAY_EXT = 0x905D

pattern GL_INT_IMAGE_1D_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_1D_EXT = 0x9057

pattern GL_INT_IMAGE_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_2D_ARRAY_EXT = 0x905E

pattern GL_INT_IMAGE_2D_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_2D_EXT = 0x9058

pattern GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY_EXT = 0x9061

pattern GL_INT_IMAGE_2D_MULTISAMPLE_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_2D_MULTISAMPLE_EXT = 0x9060

pattern GL_INT_IMAGE_2D_RECT_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_2D_RECT_EXT = 0x905A

pattern GL_INT_IMAGE_3D_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_3D_EXT = 0x9059

pattern GL_INT_IMAGE_CUBE_EXT  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_CUBE_EXT = 0x905B

pattern GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS_EXT = 0x8F39

pattern GL_MAX_IMAGE_SAMPLES_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_IMAGE_SAMPLES_EXT = 0x906D

pattern GL_MAX_IMAGE_UNITS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_IMAGE_UNITS_EXT = 0x8F38

pattern GL_PIXEL_BUFFER_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_BUFFER_BARRIER_BIT_EXT = 0x00000080

pattern GL_SHADER_IMAGE_ACCESS_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_SHADER_IMAGE_ACCESS_BARRIER_BIT_EXT = 0x00000020

pattern GL_TEXTURE_FETCH_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_FETCH_BARRIER_BIT_EXT = 0x00000008

pattern GL_TEXTURE_UPDATE_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_UPDATE_BARRIER_BIT_EXT = 0x00000100

pattern GL_TRANSFORM_FEEDBACK_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BARRIER_BIT_EXT = 0x00000800

pattern GL_UNIFORM_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_UNIFORM_BARRIER_BIT_EXT = 0x00000004

pattern GL_UNSIGNED_INT_IMAGE_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_1D_ARRAY_EXT = 0x9068

pattern GL_UNSIGNED_INT_IMAGE_1D_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_1D_EXT = 0x9062

pattern GL_UNSIGNED_INT_IMAGE_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_2D_ARRAY_EXT = 0x9069

pattern GL_UNSIGNED_INT_IMAGE_2D_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_2D_EXT = 0x9063

pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY_EXT = 0x906C

pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_EXT = 0x906B

pattern GL_UNSIGNED_INT_IMAGE_2D_RECT_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_2D_RECT_EXT = 0x9065

pattern GL_UNSIGNED_INT_IMAGE_3D_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_3D_EXT = 0x9064

pattern GL_UNSIGNED_INT_IMAGE_CUBE_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_CUBE_EXT = 0x9066

pattern GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT_EXT = 0x00000001