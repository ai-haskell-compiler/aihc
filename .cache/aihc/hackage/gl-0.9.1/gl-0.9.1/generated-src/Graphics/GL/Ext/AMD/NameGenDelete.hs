-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.NameGenDelete (
  -- * Extension Support
    gl_AMD_name_gen_delete

  -- * GL_AMD_name_gen_delete
  , glDeleteNamesAMD
  , glGenNamesAMD
  , glIsNameAMD
  , pattern GL_DATA_BUFFER_AMD
  , pattern GL_PERFORMANCE_MONITOR_AMD
  , pattern GL_QUERY_OBJECT_AMD
  , pattern GL_SAMPLER_OBJECT_AMD
  , pattern GL_VERTEX_ARRAY_OBJECT_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/name_gen_delete.txt GL_AMD_name_gen_delete> extension is available.

gl_AMD_name_gen_delete :: Bool
gl_AMD_name_gen_delete = member "GL_AMD_name_gen_delete" extensions
{-# NOINLINE gl_AMD_name_gen_delete #-}

-- | Usage: @'glDeleteNamesAMD' identifier num names@
--
-- The length of @names@ should be @num@.


glDeleteNamesAMD :: MonadIO m => GLenum -> GLuint -> Ptr GLuint -> m ()
glDeleteNamesAMD = ffienumuintPtruintIOV glDeleteNamesAMDFunPtr

glDeleteNamesAMDFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
glDeleteNamesAMDFunPtr = unsafePerformIO (getProcAddress "glDeleteNamesAMD")

{-# NOINLINE glDeleteNamesAMDFunPtr #-}

-- | Usage: @'glGenNamesAMD' identifier num names@
--
-- The length of @names@ should be @num@.


glGenNamesAMD :: MonadIO m => GLenum -> GLuint -> Ptr GLuint -> m ()
glGenNamesAMD = ffienumuintPtruintIOV glGenNamesAMDFunPtr

glGenNamesAMDFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
glGenNamesAMDFunPtr = unsafePerformIO (getProcAddress "glGenNamesAMD")

{-# NOINLINE glGenNamesAMDFunPtr #-}

-- | Usage: @'glIsNameAMD' identifier name@


glIsNameAMD :: MonadIO m => GLenum -> GLuint -> m GLboolean
glIsNameAMD = ffienumuintIOboolean glIsNameAMDFunPtr

glIsNameAMDFunPtr :: FunPtr (GLenum -> GLuint -> IO GLboolean)
glIsNameAMDFunPtr = unsafePerformIO (getProcAddress "glIsNameAMD")

{-# NOINLINE glIsNameAMDFunPtr #-}

pattern GL_DATA_BUFFER_AMD  :: (Eq a, Num a) => a

pattern GL_DATA_BUFFER_AMD = 0x9151

pattern GL_PERFORMANCE_MONITOR_AMD  :: (Eq a, Num a) => a

pattern GL_PERFORMANCE_MONITOR_AMD = 0x9152

pattern GL_QUERY_OBJECT_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_OBJECT_AMD = 0x9153

pattern GL_SAMPLER_OBJECT_AMD  :: (Eq a, Num a) => a

pattern GL_SAMPLER_OBJECT_AMD = 0x9155

pattern GL_VERTEX_ARRAY_OBJECT_AMD  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_OBJECT_AMD = 0x9154