-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.OcclusionQuery (
  -- * Extension Support
    gl_ARB_occlusion_query

  -- * GL_ARB_occlusion_query
  , glBeginQueryARB
  , glDeleteQueriesARB
  , glEndQueryARB
  , glGenQueriesARB
  , glGetQueryObjectivARB
  , glGetQueryObjectuivARB
  , glGetQueryivARB
  , glIsQueryARB
  , pattern GL_CURRENT_QUERY_ARB
  , pattern GL_QUERY_COUNTER_BITS_ARB
  , pattern GL_QUERY_RESULT_ARB
  , pattern GL_QUERY_RESULT_AVAILABLE_ARB
  , pattern GL_SAMPLES_PASSED_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/occlusion_query.txt GL_ARB_occlusion_query> extension is available.

gl_ARB_occlusion_query :: Bool
gl_ARB_occlusion_query = member "GL_ARB_occlusion_query" extensions
{-# NOINLINE gl_ARB_occlusion_query #-}

-- | Usage: @'glBeginQueryARB' target id@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBeginQuery'.


glBeginQueryARB :: MonadIO m => GLenum -> GLuint -> m ()
glBeginQueryARB = ffienumuintIOV glBeginQueryARBFunPtr

glBeginQueryARBFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBeginQueryARBFunPtr = unsafePerformIO (getProcAddress "glBeginQueryARB")

{-# NOINLINE glBeginQueryARBFunPtr #-}

-- | Usage: @'glDeleteQueriesARB' n ids@
--
-- The length of @ids@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteQueries'.


glDeleteQueriesARB :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteQueriesARB = ffisizeiPtruintIOV glDeleteQueriesARBFunPtr

glDeleteQueriesARBFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteQueriesARBFunPtr = unsafePerformIO (getProcAddress "glDeleteQueriesARB")

{-# NOINLINE glDeleteQueriesARBFunPtr #-}

-- | Usage: @'glEndQueryARB' target@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glEndQuery'.


glEndQueryARB :: MonadIO m => GLenum -> m ()
glEndQueryARB = ffienumIOV glEndQueryARBFunPtr

glEndQueryARBFunPtr :: FunPtr (GLenum -> IO ())
glEndQueryARBFunPtr = unsafePerformIO (getProcAddress "glEndQueryARB")

{-# NOINLINE glEndQueryARBFunPtr #-}

-- | Usage: @'glGenQueriesARB' n ids@
--
-- The length of @ids@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenQueries'.


glGenQueriesARB :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenQueriesARB = ffisizeiPtruintIOV glGenQueriesARBFunPtr

glGenQueriesARBFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenQueriesARBFunPtr = unsafePerformIO (getProcAddress "glGenQueriesARB")

{-# NOINLINE glGenQueriesARBFunPtr #-}

-- | Usage: @'glGetQueryObjectivARB' id pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetQueryObjectiv'.


glGetQueryObjectivARB :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetQueryObjectivARB = ffiuintenumPtrintIOV glGetQueryObjectivARBFunPtr

glGetQueryObjectivARBFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetQueryObjectivARBFunPtr = unsafePerformIO (getProcAddress "glGetQueryObjectivARB")

{-# NOINLINE glGetQueryObjectivARBFunPtr #-}

-- | Usage: @'glGetQueryObjectuivARB' id pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetQueryObjectuiv'.


glGetQueryObjectuivARB :: MonadIO m => GLuint -> GLenum -> Ptr GLuint -> m ()
glGetQueryObjectuivARB = ffiuintenumPtruintIOV glGetQueryObjectuivARBFunPtr

glGetQueryObjectuivARBFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
glGetQueryObjectuivARBFunPtr = unsafePerformIO (getProcAddress "glGetQueryObjectuivARB")

{-# NOINLINE glGetQueryObjectuivARBFunPtr #-}

-- | Usage: @'glGetQueryivARB' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetQueryiv'.


glGetQueryivARB :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetQueryivARB = ffienumenumPtrintIOV glGetQueryivARBFunPtr

glGetQueryivARBFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetQueryivARBFunPtr = unsafePerformIO (getProcAddress "glGetQueryivARB")

{-# NOINLINE glGetQueryivARBFunPtr #-}

-- | Usage: @'glIsQueryARB' id@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsQuery'.


glIsQueryARB :: MonadIO m => GLuint -> m GLboolean
glIsQueryARB = ffiuintIOboolean glIsQueryARBFunPtr

glIsQueryARBFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsQueryARBFunPtr = unsafePerformIO (getProcAddress "glIsQueryARB")

{-# NOINLINE glIsQueryARBFunPtr #-}

pattern GL_CURRENT_QUERY_ARB  :: (Eq a, Num a) => a

pattern GL_CURRENT_QUERY_ARB = 0x8865

pattern GL_QUERY_COUNTER_BITS_ARB  :: (Eq a, Num a) => a

pattern GL_QUERY_COUNTER_BITS_ARB = 0x8864

pattern GL_QUERY_RESULT_ARB  :: (Eq a, Num a) => a

pattern GL_QUERY_RESULT_ARB = 0x8866

pattern GL_QUERY_RESULT_AVAILABLE_ARB  :: (Eq a, Num a) => a

pattern GL_QUERY_RESULT_AVAILABLE_ARB = 0x8867

pattern GL_SAMPLES_PASSED_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLES_PASSED_ARB = 0x8914