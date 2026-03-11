-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.CullVertex (
  -- * Extension Support
    gl_EXT_cull_vertex

  -- * GL_EXT_cull_vertex
  , glCullParameterdvEXT
  , glCullParameterfvEXT
  , pattern GL_CULL_VERTEX_EXT
  , pattern GL_CULL_VERTEX_EYE_POSITION_EXT
  , pattern GL_CULL_VERTEX_OBJECT_POSITION_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/cull_vertex.txt GL_EXT_cull_vertex> extension is available.

gl_EXT_cull_vertex :: Bool
gl_EXT_cull_vertex = member "GL_EXT_cull_vertex" extensions
{-# NOINLINE gl_EXT_cull_vertex #-}

-- | Usage: @'glCullParameterdvEXT' pname params@
--
-- The parameter @pname@ is a @CullParameterEXT@.
--
-- The length of @params@ should be @4@.


glCullParameterdvEXT :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glCullParameterdvEXT = ffienumPtrdoubleIOV glCullParameterdvEXTFunPtr

glCullParameterdvEXTFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glCullParameterdvEXTFunPtr = unsafePerformIO (getProcAddress "glCullParameterdvEXT")

{-# NOINLINE glCullParameterdvEXTFunPtr #-}

-- | Usage: @'glCullParameterfvEXT' pname params@
--
-- The parameter @pname@ is a @CullParameterEXT@.
--
-- The length of @params@ should be @4@.


glCullParameterfvEXT :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glCullParameterfvEXT = ffienumPtrfloatIOV glCullParameterfvEXTFunPtr

glCullParameterfvEXTFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glCullParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glCullParameterfvEXT")

{-# NOINLINE glCullParameterfvEXTFunPtr #-}

pattern GL_CULL_VERTEX_EXT  :: (Eq a, Num a) => a

pattern GL_CULL_VERTEX_EXT = 0x81AA

pattern GL_CULL_VERTEX_EYE_POSITION_EXT  :: (Eq a, Num a) => a

pattern GL_CULL_VERTEX_EYE_POSITION_EXT = 0x81AB

pattern GL_CULL_VERTEX_OBJECT_POSITION_EXT  :: (Eq a, Num a) => a

pattern GL_CULL_VERTEX_OBJECT_POSITION_EXT = 0x81AC