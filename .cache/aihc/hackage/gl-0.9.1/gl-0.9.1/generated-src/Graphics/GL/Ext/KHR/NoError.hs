-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.NoError (
  -- * Extension Support
    gl_KHR_no_error

  -- * GL_KHR_no_error
  , pattern GL_CONTEXT_FLAG_NO_ERROR_BIT_KHR
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/KHR/no_error.txt GL_KHR_no_error> extension is available.

gl_KHR_no_error :: Bool
gl_KHR_no_error = member "GL_KHR_no_error" extensions
{-# NOINLINE gl_KHR_no_error #-}

pattern GL_CONTEXT_FLAG_NO_ERROR_BIT_KHR  :: (Eq a, Num a) => a

pattern GL_CONTEXT_FLAG_NO_ERROR_BIT_KHR = 0x00000008