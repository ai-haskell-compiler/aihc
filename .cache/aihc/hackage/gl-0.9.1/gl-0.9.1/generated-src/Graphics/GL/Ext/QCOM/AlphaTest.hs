-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.QCOM.AlphaTest (
  -- * Extension Support
    gl_QCOM_alpha_test

  -- * GL_QCOM_alpha_test
  , glAlphaFuncQCOM
  , pattern GL_ALPHA_TEST_FUNC_QCOM
  , pattern GL_ALPHA_TEST_QCOM
  , pattern GL_ALPHA_TEST_REF_QCOM
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/QCOM/QCOM_alpha_test.txt GL_QCOM_alpha_test> extension is available.

gl_QCOM_alpha_test :: Bool
gl_QCOM_alpha_test = member "GL_QCOM_alpha_test" extensions
{-# NOINLINE gl_QCOM_alpha_test #-}

-- | Usage: @'glAlphaFuncQCOM' func ref@


glAlphaFuncQCOM :: MonadIO m => GLenum -> GLclampf -> m ()
glAlphaFuncQCOM = ffienumclampfIOV glAlphaFuncQCOMFunPtr

glAlphaFuncQCOMFunPtr :: FunPtr (GLenum -> GLclampf -> IO ())
glAlphaFuncQCOMFunPtr = unsafePerformIO (getProcAddress "glAlphaFuncQCOM")

{-# NOINLINE glAlphaFuncQCOMFunPtr #-}

pattern GL_ALPHA_TEST_FUNC_QCOM  :: (Eq a, Num a) => a

pattern GL_ALPHA_TEST_FUNC_QCOM = 0x0BC1

pattern GL_ALPHA_TEST_QCOM  :: (Eq a, Num a) => a

pattern GL_ALPHA_TEST_QCOM = 0x0BC0

pattern GL_ALPHA_TEST_REF_QCOM  :: (Eq a, Num a) => a

pattern GL_ALPHA_TEST_REF_QCOM = 0x0BC2