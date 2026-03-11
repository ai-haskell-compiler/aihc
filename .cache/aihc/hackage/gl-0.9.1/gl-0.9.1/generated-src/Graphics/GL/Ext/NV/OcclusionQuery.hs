-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.OcclusionQuery (
  -- * Extension Support
    gl_NV_occlusion_query

  -- * GL_NV_occlusion_query
  , glBeginOcclusionQueryNV
  , glDeleteOcclusionQueriesNV
  , glEndOcclusionQueryNV
  , glGenOcclusionQueriesNV
  , glGetOcclusionQueryivNV
  , glGetOcclusionQueryuivNV
  , glIsOcclusionQueryNV
  , pattern GL_CURRENT_OCCLUSION_QUERY_ID_NV
  , pattern GL_PIXEL_COUNTER_BITS_NV
  , pattern GL_PIXEL_COUNT_AVAILABLE_NV
  , pattern GL_PIXEL_COUNT_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/occlusion_query.txt GL_NV_occlusion_query> extension is available.

gl_NV_occlusion_query :: Bool
gl_NV_occlusion_query = member "GL_NV_occlusion_query" extensions
{-# NOINLINE gl_NV_occlusion_query #-}

-- | Usage: @'glBeginOcclusionQueryNV' id@


glBeginOcclusionQueryNV :: MonadIO m => GLuint -> m ()
glBeginOcclusionQueryNV = ffiuintIOV glBeginOcclusionQueryNVFunPtr

glBeginOcclusionQueryNVFunPtr :: FunPtr (GLuint -> IO ())
glBeginOcclusionQueryNVFunPtr = unsafePerformIO (getProcAddress "glBeginOcclusionQueryNV")

{-# NOINLINE glBeginOcclusionQueryNVFunPtr #-}

-- | Usage: @'glDeleteOcclusionQueriesNV' n ids@
--
-- The length of @ids@ should be @n@.


glDeleteOcclusionQueriesNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteOcclusionQueriesNV = ffisizeiPtruintIOV glDeleteOcclusionQueriesNVFunPtr

glDeleteOcclusionQueriesNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteOcclusionQueriesNVFunPtr = unsafePerformIO (getProcAddress "glDeleteOcclusionQueriesNV")

{-# NOINLINE glDeleteOcclusionQueriesNVFunPtr #-}

-- | Usage: @'glEndOcclusionQueryNV'@


glEndOcclusionQueryNV :: MonadIO m => m ()
glEndOcclusionQueryNV = ffiIOV glEndOcclusionQueryNVFunPtr

glEndOcclusionQueryNVFunPtr :: FunPtr (IO ())
glEndOcclusionQueryNVFunPtr = unsafePerformIO (getProcAddress "glEndOcclusionQueryNV")

{-# NOINLINE glEndOcclusionQueryNVFunPtr #-}

-- | Usage: @'glGenOcclusionQueriesNV' n ids@
--
-- The length of @ids@ should be @n@.


glGenOcclusionQueriesNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenOcclusionQueriesNV = ffisizeiPtruintIOV glGenOcclusionQueriesNVFunPtr

glGenOcclusionQueriesNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenOcclusionQueriesNVFunPtr = unsafePerformIO (getProcAddress "glGenOcclusionQueriesNV")

{-# NOINLINE glGenOcclusionQueriesNVFunPtr #-}

-- | Usage: @'glGetOcclusionQueryivNV' id pname params@
--
-- The parameter @pname@ is a @OcclusionQueryParameterNameNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetOcclusionQueryivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetOcclusionQueryivNV = ffiuintenumPtrintIOV glGetOcclusionQueryivNVFunPtr

glGetOcclusionQueryivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetOcclusionQueryivNVFunPtr = unsafePerformIO (getProcAddress "glGetOcclusionQueryivNV")

{-# NOINLINE glGetOcclusionQueryivNVFunPtr #-}

-- | Usage: @'glGetOcclusionQueryuivNV' id pname params@
--
-- The parameter @pname@ is a @OcclusionQueryParameterNameNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetOcclusionQueryuivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLuint -> m ()
glGetOcclusionQueryuivNV = ffiuintenumPtruintIOV glGetOcclusionQueryuivNVFunPtr

glGetOcclusionQueryuivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
glGetOcclusionQueryuivNVFunPtr = unsafePerformIO (getProcAddress "glGetOcclusionQueryuivNV")

{-# NOINLINE glGetOcclusionQueryuivNVFunPtr #-}

-- | Usage: @'glIsOcclusionQueryNV' id@


glIsOcclusionQueryNV :: MonadIO m => GLuint -> m GLboolean
glIsOcclusionQueryNV = ffiuintIOboolean glIsOcclusionQueryNVFunPtr

glIsOcclusionQueryNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsOcclusionQueryNVFunPtr = unsafePerformIO (getProcAddress "glIsOcclusionQueryNV")

{-# NOINLINE glIsOcclusionQueryNVFunPtr #-}

pattern GL_CURRENT_OCCLUSION_QUERY_ID_NV  :: (Eq a, Num a) => a

pattern GL_CURRENT_OCCLUSION_QUERY_ID_NV = 0x8865

pattern GL_PIXEL_COUNTER_BITS_NV  :: (Eq a, Num a) => a

pattern GL_PIXEL_COUNTER_BITS_NV = 0x8864

pattern GL_PIXEL_COUNT_AVAILABLE_NV  :: (Eq a, Num a) => a

pattern GL_PIXEL_COUNT_AVAILABLE_NV = 0x8867

pattern GL_PIXEL_COUNT_NV  :: (Eq a, Num a) => a

pattern GL_PIXEL_COUNT_NV = 0x8866