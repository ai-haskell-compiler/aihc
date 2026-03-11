-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ParallelShaderCompile (
  -- * Extension Support
    gl_ARB_parallel_shader_compile

  -- * GL_ARB_parallel_shader_compile
  , glMaxShaderCompilerThreadsARB
  , pattern GL_COMPLETION_STATUS_ARB
  , pattern GL_MAX_SHADER_COMPILER_THREADS_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/parallel_shader_compile.txt GL_ARB_parallel_shader_compile> extension is available.

gl_ARB_parallel_shader_compile :: Bool
gl_ARB_parallel_shader_compile = member "GL_ARB_parallel_shader_compile" extensions
{-# NOINLINE gl_ARB_parallel_shader_compile #-}

-- | Usage: @'glMaxShaderCompilerThreadsARB' count@


glMaxShaderCompilerThreadsARB :: MonadIO m => GLuint -> m ()
glMaxShaderCompilerThreadsARB = ffiuintIOV glMaxShaderCompilerThreadsARBFunPtr

glMaxShaderCompilerThreadsARBFunPtr :: FunPtr (GLuint -> IO ())
glMaxShaderCompilerThreadsARBFunPtr = unsafePerformIO (getProcAddress "glMaxShaderCompilerThreadsARB")

{-# NOINLINE glMaxShaderCompilerThreadsARBFunPtr #-}

pattern GL_COMPLETION_STATUS_ARB  :: (Eq a, Num a) => a

pattern GL_COMPLETION_STATUS_ARB = 0x91B1

pattern GL_MAX_SHADER_COMPILER_THREADS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_SHADER_COMPILER_THREADS_ARB = 0x91B0