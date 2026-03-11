-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DisjointTimerQuery (
  -- * Extension Support
    gl_EXT_disjoint_timer_query

  -- * GL_EXT_disjoint_timer_query
  , glBeginQueryEXT
  , glDeleteQueriesEXT
  , glEndQueryEXT
  , glGenQueriesEXT
  , glGetQueryObjecti64vEXT
  , glGetQueryObjectivEXT
  , glGetQueryObjectui64vEXT
  , glGetQueryObjectuivEXT
  , glGetQueryivEXT
  , glIsQueryEXT
  , glQueryCounterEXT
  , pattern GL_CURRENT_QUERY_EXT
  , pattern GL_GPU_DISJOINT_EXT
  , pattern GL_QUERY_COUNTER_BITS_EXT
  , pattern GL_QUERY_RESULT_AVAILABLE_EXT
  , pattern GL_QUERY_RESULT_EXT
  , pattern GL_TIMESTAMP_EXT
  , pattern GL_TIME_ELAPSED_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_disjoint_timer_query.txt GL_EXT_disjoint_timer_query> extension is available.

gl_EXT_disjoint_timer_query :: Bool
gl_EXT_disjoint_timer_query = member "GL_EXT_disjoint_timer_query" extensions
{-# NOINLINE gl_EXT_disjoint_timer_query #-}

-- | Usage: @'glGetQueryObjectivEXT' id pname params@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetQueryObjectiv'.


glGetQueryObjectivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetQueryObjectivEXT = ffiuintenumPtrintIOV glGetQueryObjectivEXTFunPtr

glGetQueryObjectivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetQueryObjectivEXTFunPtr = unsafePerformIO (getProcAddress "glGetQueryObjectivEXT")

{-# NOINLINE glGetQueryObjectivEXTFunPtr #-}

-- | Usage: @'glQueryCounterEXT' id target@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glQueryCounter'.


glQueryCounterEXT :: MonadIO m => GLuint -> GLenum -> m ()
glQueryCounterEXT = ffiuintenumIOV glQueryCounterEXTFunPtr

glQueryCounterEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glQueryCounterEXTFunPtr = unsafePerformIO (getProcAddress "glQueryCounterEXT")

{-# NOINLINE glQueryCounterEXTFunPtr #-}

pattern GL_GPU_DISJOINT_EXT  :: (Eq a, Num a) => a

pattern GL_GPU_DISJOINT_EXT = 0x8FBB

pattern GL_QUERY_COUNTER_BITS_EXT  :: (Eq a, Num a) => a

pattern GL_QUERY_COUNTER_BITS_EXT = 0x8864

pattern GL_TIMESTAMP_EXT  :: (Eq a, Num a) => a

pattern GL_TIMESTAMP_EXT = 0x8E28