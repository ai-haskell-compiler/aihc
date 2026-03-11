-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.StencilOperationExtended (
  -- * Extension Support
    gl_AMD_stencil_operation_extended

  -- * GL_AMD_stencil_operation_extended
  , glStencilOpValueAMD
  , pattern GL_REPLACE_VALUE_AMD
  , pattern GL_SET_AMD
  , pattern GL_STENCIL_BACK_OP_VALUE_AMD
  , pattern GL_STENCIL_OP_VALUE_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/stencil_operation_extended.txt GL_AMD_stencil_operation_extended> extension is available.

gl_AMD_stencil_operation_extended :: Bool
gl_AMD_stencil_operation_extended = member "GL_AMD_stencil_operation_extended" extensions
{-# NOINLINE gl_AMD_stencil_operation_extended #-}

-- | Usage: @'glStencilOpValueAMD' face value@
--
-- The parameter @face@ is a @StencilFaceDirection@.


glStencilOpValueAMD :: MonadIO m => GLenum -> GLuint -> m ()
glStencilOpValueAMD = ffienumuintIOV glStencilOpValueAMDFunPtr

glStencilOpValueAMDFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glStencilOpValueAMDFunPtr = unsafePerformIO (getProcAddress "glStencilOpValueAMD")

{-# NOINLINE glStencilOpValueAMDFunPtr #-}

pattern GL_REPLACE_VALUE_AMD  :: (Eq a, Num a) => a

pattern GL_REPLACE_VALUE_AMD = 0x874B

pattern GL_SET_AMD  :: (Eq a, Num a) => a

pattern GL_SET_AMD = 0x874A

pattern GL_STENCIL_BACK_OP_VALUE_AMD  :: (Eq a, Num a) => a

pattern GL_STENCIL_BACK_OP_VALUE_AMD = 0x874D

pattern GL_STENCIL_OP_VALUE_AMD  :: (Eq a, Num a) => a

pattern GL_STENCIL_OP_VALUE_AMD = 0x874C