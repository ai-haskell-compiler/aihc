-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.RegisterCombiners2 (
  -- * Extension Support
    gl_NV_register_combiners2

  -- * GL_NV_register_combiners2
  , glCombinerStageParameterfvNV
  , glGetCombinerStageParameterfvNV
  , pattern GL_PER_STAGE_CONSTANTS_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/register_combiners2.txt GL_NV_register_combiners2> extension is available.

gl_NV_register_combiners2 :: Bool
gl_NV_register_combiners2 = member "GL_NV_register_combiners2" extensions
{-# NOINLINE gl_NV_register_combiners2 #-}

-- | Usage: @'glCombinerStageParameterfvNV' stage pname params@
--
-- The parameter @stage@ is a @CombinerStageNV@.
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glCombinerStageParameterfvNV :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glCombinerStageParameterfvNV = ffienumenumPtrfloatIOV glCombinerStageParameterfvNVFunPtr

glCombinerStageParameterfvNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glCombinerStageParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glCombinerStageParameterfvNV")

{-# NOINLINE glCombinerStageParameterfvNVFunPtr #-}

-- | Usage: @'glGetCombinerStageParameterfvNV' stage pname params@
--
-- The parameter @stage@ is a @CombinerStageNV@.
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetCombinerStageParameterfvNV :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetCombinerStageParameterfvNV = ffienumenumPtrfloatIOV glGetCombinerStageParameterfvNVFunPtr

glGetCombinerStageParameterfvNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetCombinerStageParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetCombinerStageParameterfvNV")

{-# NOINLINE glGetCombinerStageParameterfvNVFunPtr #-}

pattern GL_PER_STAGE_CONSTANTS_NV  :: (Eq a, Num a) => a

pattern GL_PER_STAGE_CONSTANTS_NV = 0x8535