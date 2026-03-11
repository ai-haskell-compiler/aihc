-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.SeparateStencil (
  -- * Extension Support
    gl_ATI_separate_stencil

  -- * GL_ATI_separate_stencil
  , glStencilFuncSeparateATI
  , glStencilOpSeparateATI
  , pattern GL_STENCIL_BACK_FAIL_ATI
  , pattern GL_STENCIL_BACK_FUNC_ATI
  , pattern GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI
  , pattern GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/separate_stencil.txt GL_ATI_separate_stencil> extension is available.

gl_ATI_separate_stencil :: Bool
gl_ATI_separate_stencil = member "GL_ATI_separate_stencil" extensions
{-# NOINLINE gl_ATI_separate_stencil #-}

-- | Usage: @'glStencilFuncSeparateATI' frontfunc backfunc ref mask@
--
-- The parameter @frontfunc@ is a @StencilFunction@, one of: 'Graphics.GL.Internal.Shared.GL_ALWAYS', 'Graphics.GL.Internal.Shared.GL_EQUAL', 'Graphics.GL.Internal.Shared.GL_GEQUAL', 'Graphics.GL.Internal.Shared.GL_GREATER', 'Graphics.GL.Internal.Shared.GL_LEQUAL', 'Graphics.GL.Internal.Shared.GL_LESS', 'Graphics.GL.Internal.Shared.GL_NEVER', 'Graphics.GL.Internal.Shared.GL_NOTEQUAL'.
--
-- The parameter @backfunc@ is a @StencilFunction@, one of: 'Graphics.GL.Internal.Shared.GL_ALWAYS', 'Graphics.GL.Internal.Shared.GL_EQUAL', 'Graphics.GL.Internal.Shared.GL_GEQUAL', 'Graphics.GL.Internal.Shared.GL_GREATER', 'Graphics.GL.Internal.Shared.GL_LEQUAL', 'Graphics.GL.Internal.Shared.GL_LESS', 'Graphics.GL.Internal.Shared.GL_NEVER', 'Graphics.GL.Internal.Shared.GL_NOTEQUAL'.
--
-- The parameter @ref@ is a @ClampedStencilValue@.
--
-- The parameter @mask@ is a @MaskedStencilValue@.


glStencilFuncSeparateATI :: MonadIO m => GLenum -> GLenum -> GLint -> GLuint -> m ()
glStencilFuncSeparateATI = ffienumenumintuintIOV glStencilFuncSeparateATIFunPtr

glStencilFuncSeparateATIFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLuint -> IO ())
glStencilFuncSeparateATIFunPtr = unsafePerformIO (getProcAddress "glStencilFuncSeparateATI")

{-# NOINLINE glStencilFuncSeparateATIFunPtr #-}

-- | Usage: @'glStencilOpSeparateATI' face sfail dpfail dppass@
--
-- The parameter @face@ is a @StencilFaceDirection@.
--
-- The parameter @sfail@ is a @StencilOp@, one of: 'Graphics.GL.Internal.Shared.GL_DECR', 'Graphics.GL.Internal.Shared.GL_INCR', 'Graphics.GL.Internal.Shared.GL_INVERT', 'Graphics.GL.Internal.Shared.GL_KEEP', 'Graphics.GL.Internal.Shared.GL_REPLACE', 'Graphics.GL.Internal.Shared.GL_ZERO'.
--
-- The parameter @dpfail@ is a @StencilOp@, one of: 'Graphics.GL.Internal.Shared.GL_DECR', 'Graphics.GL.Internal.Shared.GL_INCR', 'Graphics.GL.Internal.Shared.GL_INVERT', 'Graphics.GL.Internal.Shared.GL_KEEP', 'Graphics.GL.Internal.Shared.GL_REPLACE', 'Graphics.GL.Internal.Shared.GL_ZERO'.
--
-- The parameter @dppass@ is a @StencilOp@, one of: 'Graphics.GL.Internal.Shared.GL_DECR', 'Graphics.GL.Internal.Shared.GL_INCR', 'Graphics.GL.Internal.Shared.GL_INVERT', 'Graphics.GL.Internal.Shared.GL_KEEP', 'Graphics.GL.Internal.Shared.GL_REPLACE', 'Graphics.GL.Internal.Shared.GL_ZERO'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glStencilOpSeparate'.


glStencilOpSeparateATI :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> m ()
glStencilOpSeparateATI = ffienumenumenumenumIOV glStencilOpSeparateATIFunPtr

glStencilOpSeparateATIFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glStencilOpSeparateATIFunPtr = unsafePerformIO (getProcAddress "glStencilOpSeparateATI")

{-# NOINLINE glStencilOpSeparateATIFunPtr #-}

pattern GL_STENCIL_BACK_FAIL_ATI  :: (Eq a, Num a) => a

pattern GL_STENCIL_BACK_FAIL_ATI = 0x8801

pattern GL_STENCIL_BACK_FUNC_ATI  :: (Eq a, Num a) => a

pattern GL_STENCIL_BACK_FUNC_ATI = 0x8800

pattern GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI  :: (Eq a, Num a) => a

pattern GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI = 0x8802

pattern GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI  :: (Eq a, Num a) => a

pattern GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI = 0x8803