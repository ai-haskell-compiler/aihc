-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ComputeVariableGroupSize (
  -- * Extension Support
    gl_ARB_compute_variable_group_size

  -- * GL_ARB_compute_variable_group_size
  , glDispatchComputeGroupSizeARB
  , pattern GL_MAX_COMPUTE_FIXED_GROUP_INVOCATIONS_ARB
  , pattern GL_MAX_COMPUTE_FIXED_GROUP_SIZE_ARB
  , pattern GL_MAX_COMPUTE_VARIABLE_GROUP_INVOCATIONS_ARB
  , pattern GL_MAX_COMPUTE_VARIABLE_GROUP_SIZE_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/compute_variable_group_size.txt GL_ARB_compute_variable_group_size> extension is available.

gl_ARB_compute_variable_group_size :: Bool
gl_ARB_compute_variable_group_size = member "GL_ARB_compute_variable_group_size" extensions
{-# NOINLINE gl_ARB_compute_variable_group_size #-}

-- | Usage: @'glDispatchComputeGroupSizeARB' num_groups_x num_groups_y num_groups_z group_size_x group_size_y group_size_z@


glDispatchComputeGroupSizeARB :: MonadIO m => GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glDispatchComputeGroupSizeARB = ffiuintuintuintuintuintuintIOV glDispatchComputeGroupSizeARBFunPtr

glDispatchComputeGroupSizeARBFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glDispatchComputeGroupSizeARBFunPtr = unsafePerformIO (getProcAddress "glDispatchComputeGroupSizeARB")

{-# NOINLINE glDispatchComputeGroupSizeARBFunPtr #-}

pattern GL_MAX_COMPUTE_FIXED_GROUP_INVOCATIONS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_COMPUTE_FIXED_GROUP_INVOCATIONS_ARB = 0x90EB

pattern GL_MAX_COMPUTE_FIXED_GROUP_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_COMPUTE_FIXED_GROUP_SIZE_ARB = 0x91BF

pattern GL_MAX_COMPUTE_VARIABLE_GROUP_INVOCATIONS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_COMPUTE_VARIABLE_GROUP_INVOCATIONS_ARB = 0x9344

pattern GL_MAX_COMPUTE_VARIABLE_GROUP_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_COMPUTE_VARIABLE_GROUP_SIZE_ARB = 0x9345