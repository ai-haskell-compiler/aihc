-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.VertexArrayObject (
  -- * Extension Support
    gl_APPLE_vertex_array_object

  -- * GL_APPLE_vertex_array_object
  , glBindVertexArrayAPPLE
  , glDeleteVertexArraysAPPLE
  , glGenVertexArraysAPPLE
  , glIsVertexArrayAPPLE
  , pattern GL_VERTEX_ARRAY_BINDING_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/vertex_array_object.txt GL_APPLE_vertex_array_object> extension is available.

gl_APPLE_vertex_array_object :: Bool
gl_APPLE_vertex_array_object = member "GL_APPLE_vertex_array_object" extensions
{-# NOINLINE gl_APPLE_vertex_array_object #-}

-- | Usage: @'glBindVertexArrayAPPLE' array@


glBindVertexArrayAPPLE :: MonadIO m => GLuint -> m ()
glBindVertexArrayAPPLE = ffiuintIOV glBindVertexArrayAPPLEFunPtr

glBindVertexArrayAPPLEFunPtr :: FunPtr (GLuint -> IO ())
glBindVertexArrayAPPLEFunPtr = unsafePerformIO (getProcAddress "glBindVertexArrayAPPLE")

{-# NOINLINE glBindVertexArrayAPPLEFunPtr #-}

-- | Usage: @'glDeleteVertexArraysAPPLE' n arrays@
--
-- The length of @arrays@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteVertexArrays'.


glDeleteVertexArraysAPPLE :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteVertexArraysAPPLE = ffisizeiPtruintIOV glDeleteVertexArraysAPPLEFunPtr

glDeleteVertexArraysAPPLEFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteVertexArraysAPPLEFunPtr = unsafePerformIO (getProcAddress "glDeleteVertexArraysAPPLE")

{-# NOINLINE glDeleteVertexArraysAPPLEFunPtr #-}

-- | Usage: @'glGenVertexArraysAPPLE' n arrays@
--
-- The length of @arrays@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenVertexArrays'.


glGenVertexArraysAPPLE :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenVertexArraysAPPLE = ffisizeiPtruintIOV glGenVertexArraysAPPLEFunPtr

glGenVertexArraysAPPLEFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenVertexArraysAPPLEFunPtr = unsafePerformIO (getProcAddress "glGenVertexArraysAPPLE")

{-# NOINLINE glGenVertexArraysAPPLEFunPtr #-}

-- | Usage: @'glIsVertexArrayAPPLE' array@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsVertexArray'.


glIsVertexArrayAPPLE :: MonadIO m => GLuint -> m GLboolean
glIsVertexArrayAPPLE = ffiuintIOboolean glIsVertexArrayAPPLEFunPtr

glIsVertexArrayAPPLEFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsVertexArrayAPPLEFunPtr = unsafePerformIO (getProcAddress "glIsVertexArrayAPPLE")

{-# NOINLINE glIsVertexArrayAPPLEFunPtr #-}

pattern GL_VERTEX_ARRAY_BINDING_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_BINDING_APPLE = 0x85B5