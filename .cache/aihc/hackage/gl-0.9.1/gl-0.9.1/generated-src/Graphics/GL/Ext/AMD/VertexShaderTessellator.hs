-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.VertexShaderTessellator (
  -- * Extension Support
    gl_AMD_vertex_shader_tessellator

  -- * GL_AMD_vertex_shader_tessellator
  , glTessellationFactorAMD
  , glTessellationModeAMD
  , pattern GL_CONTINUOUS_AMD
  , pattern GL_DISCRETE_AMD
  , pattern GL_INT_SAMPLER_BUFFER_AMD
  , pattern GL_SAMPLER_BUFFER_AMD
  , pattern GL_TESSELLATION_FACTOR_AMD
  , pattern GL_TESSELLATION_MODE_AMD
  , pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/vertex_shader_tessellator.txt GL_AMD_vertex_shader_tessellator> extension is available.

gl_AMD_vertex_shader_tessellator :: Bool
gl_AMD_vertex_shader_tessellator = member "GL_AMD_vertex_shader_tessellator" extensions
{-# NOINLINE gl_AMD_vertex_shader_tessellator #-}

-- | Usage: @'glTessellationFactorAMD' factor@


glTessellationFactorAMD :: MonadIO m => GLfloat -> m ()
glTessellationFactorAMD = ffifloatIOV glTessellationFactorAMDFunPtr

glTessellationFactorAMDFunPtr :: FunPtr (GLfloat -> IO ())
glTessellationFactorAMDFunPtr = unsafePerformIO (getProcAddress "glTessellationFactorAMD")

{-# NOINLINE glTessellationFactorAMDFunPtr #-}

-- | Usage: @'glTessellationModeAMD' mode@


glTessellationModeAMD :: MonadIO m => GLenum -> m ()
glTessellationModeAMD = ffienumIOV glTessellationModeAMDFunPtr

glTessellationModeAMDFunPtr :: FunPtr (GLenum -> IO ())
glTessellationModeAMDFunPtr = unsafePerformIO (getProcAddress "glTessellationModeAMD")

{-# NOINLINE glTessellationModeAMDFunPtr #-}

pattern GL_CONTINUOUS_AMD  :: (Eq a, Num a) => a

pattern GL_CONTINUOUS_AMD = 0x9007

pattern GL_DISCRETE_AMD  :: (Eq a, Num a) => a

pattern GL_DISCRETE_AMD = 0x9006

pattern GL_INT_SAMPLER_BUFFER_AMD  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_BUFFER_AMD = 0x9002

pattern GL_SAMPLER_BUFFER_AMD  :: (Eq a, Num a) => a

pattern GL_SAMPLER_BUFFER_AMD = 0x9001

pattern GL_TESSELLATION_FACTOR_AMD  :: (Eq a, Num a) => a

pattern GL_TESSELLATION_FACTOR_AMD = 0x9005

pattern GL_TESSELLATION_MODE_AMD  :: (Eq a, Num a) => a

pattern GL_TESSELLATION_MODE_AMD = 0x9004

pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_AMD  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_AMD = 0x9003