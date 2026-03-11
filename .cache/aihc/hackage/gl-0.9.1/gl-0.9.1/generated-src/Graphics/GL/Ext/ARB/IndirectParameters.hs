-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.IndirectParameters (
  -- * Extension Support
    gl_ARB_indirect_parameters

  -- * GL_ARB_indirect_parameters
  , glMultiDrawArraysIndirectCountARB
  , glMultiDrawElementsIndirectCountARB
  , pattern GL_PARAMETER_BUFFER_ARB
  , pattern GL_PARAMETER_BUFFER_BINDING_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/indirect_parameters.txt GL_ARB_indirect_parameters> extension is available.

gl_ARB_indirect_parameters :: Bool
gl_ARB_indirect_parameters = member "GL_ARB_indirect_parameters" extensions
{-# NOINLINE gl_ARB_indirect_parameters #-}

-- | Usage: @'glMultiDrawArraysIndirectCountARB' mode indirect drawcount maxdrawcount stride@


glMultiDrawArraysIndirectCountARB :: MonadIO m => GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> m ()
glMultiDrawArraysIndirectCountARB = ffienumintptrintptrsizeisizeiIOV glMultiDrawArraysIndirectCountARBFunPtr

glMultiDrawArraysIndirectCountARBFunPtr :: FunPtr (GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ())
glMultiDrawArraysIndirectCountARBFunPtr = unsafePerformIO (getProcAddress "glMultiDrawArraysIndirectCountARB")

{-# NOINLINE glMultiDrawArraysIndirectCountARBFunPtr #-}

-- | Usage: @'glMultiDrawElementsIndirectCountARB' mode type indirect drawcount maxdrawcount stride@


glMultiDrawElementsIndirectCountARB :: MonadIO m => GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> m ()
glMultiDrawElementsIndirectCountARB = ffienumenumintptrintptrsizeisizeiIOV glMultiDrawElementsIndirectCountARBFunPtr

glMultiDrawElementsIndirectCountARBFunPtr :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ())
glMultiDrawElementsIndirectCountARBFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementsIndirectCountARB")

{-# NOINLINE glMultiDrawElementsIndirectCountARBFunPtr #-}

pattern GL_PARAMETER_BUFFER_ARB  :: (Eq a, Num a) => a

pattern GL_PARAMETER_BUFFER_ARB = 0x80EE

pattern GL_PARAMETER_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_PARAMETER_BUFFER_BINDING_ARB = 0x80EF