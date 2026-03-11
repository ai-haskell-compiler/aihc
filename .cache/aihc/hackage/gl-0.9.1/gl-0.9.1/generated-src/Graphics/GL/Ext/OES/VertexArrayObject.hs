-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.VertexArrayObject (
  -- * Extension Support
    gl_OES_vertex_array_object

  -- * GL_OES_vertex_array_object
  , glBindVertexArrayOES
  , glDeleteVertexArraysOES
  , glGenVertexArraysOES
  , glIsVertexArrayOES
  , pattern GL_VERTEX_ARRAY_BINDING_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_vertex_array_object.txt GL_OES_vertex_array_object> extension is available.

gl_OES_vertex_array_object :: Bool
gl_OES_vertex_array_object = member "GL_OES_vertex_array_object" extensions
{-# NOINLINE gl_OES_vertex_array_object #-}

-- | Usage: @'glBindVertexArrayOES' array@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindVertexArray'.


glBindVertexArrayOES :: MonadIO m => GLuint -> m ()
glBindVertexArrayOES = ffiuintIOV glBindVertexArrayOESFunPtr

glBindVertexArrayOESFunPtr :: FunPtr (GLuint -> IO ())
glBindVertexArrayOESFunPtr = unsafePerformIO (getProcAddress "glBindVertexArrayOES")

{-# NOINLINE glBindVertexArrayOESFunPtr #-}

-- | Usage: @'glDeleteVertexArraysOES' n arrays@
--
-- The length of @arrays@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteVertexArrays'.


glDeleteVertexArraysOES :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteVertexArraysOES = ffisizeiPtruintIOV glDeleteVertexArraysOESFunPtr

glDeleteVertexArraysOESFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteVertexArraysOESFunPtr = unsafePerformIO (getProcAddress "glDeleteVertexArraysOES")

{-# NOINLINE glDeleteVertexArraysOESFunPtr #-}

-- | Usage: @'glGenVertexArraysOES' n arrays@
--
-- The length of @arrays@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenVertexArrays'.


glGenVertexArraysOES :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenVertexArraysOES = ffisizeiPtruintIOV glGenVertexArraysOESFunPtr

glGenVertexArraysOESFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenVertexArraysOESFunPtr = unsafePerformIO (getProcAddress "glGenVertexArraysOES")

{-# NOINLINE glGenVertexArraysOESFunPtr #-}

-- | Usage: @'glIsVertexArrayOES' array@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsVertexArray'.


glIsVertexArrayOES :: MonadIO m => GLuint -> m GLboolean
glIsVertexArrayOES = ffiuintIOboolean glIsVertexArrayOESFunPtr

glIsVertexArrayOESFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsVertexArrayOESFunPtr = unsafePerformIO (getProcAddress "glIsVertexArrayOES")

{-# NOINLINE glIsVertexArrayOESFunPtr #-}

pattern GL_VERTEX_ARRAY_BINDING_OES  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_BINDING_OES = 0x85B5