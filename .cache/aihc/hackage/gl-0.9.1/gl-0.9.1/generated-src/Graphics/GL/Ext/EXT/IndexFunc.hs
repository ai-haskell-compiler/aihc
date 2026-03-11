-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.IndexFunc (
  -- * Extension Support
    gl_EXT_index_func

  -- * GL_EXT_index_func
  , glIndexFuncEXT
  , pattern GL_INDEX_TEST_EXT
  , pattern GL_INDEX_TEST_FUNC_EXT
  , pattern GL_INDEX_TEST_REF_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/index_func.txt GL_EXT_index_func> extension is available.

gl_EXT_index_func :: Bool
gl_EXT_index_func = member "GL_EXT_index_func" extensions
{-# NOINLINE gl_EXT_index_func #-}

-- | Usage: @'glIndexFuncEXT' func ref@
--
-- The parameter @func@ is a @IndexFunctionEXT@.
--
-- The parameter @ref@ is a @ClampedFloat32@.


glIndexFuncEXT :: MonadIO m => GLenum -> GLclampf -> m ()
glIndexFuncEXT = ffienumclampfIOV glIndexFuncEXTFunPtr

glIndexFuncEXTFunPtr :: FunPtr (GLenum -> GLclampf -> IO ())
glIndexFuncEXTFunPtr = unsafePerformIO (getProcAddress "glIndexFuncEXT")

{-# NOINLINE glIndexFuncEXTFunPtr #-}

pattern GL_INDEX_TEST_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_TEST_EXT = 0x81B5

pattern GL_INDEX_TEST_FUNC_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_TEST_FUNC_EXT = 0x81B6

pattern GL_INDEX_TEST_REF_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_TEST_REF_EXT = 0x81B7