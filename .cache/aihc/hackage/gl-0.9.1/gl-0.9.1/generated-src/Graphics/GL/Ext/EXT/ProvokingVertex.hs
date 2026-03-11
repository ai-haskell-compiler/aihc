-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ProvokingVertex (
  -- * Extension Support
    gl_EXT_provoking_vertex

  -- * GL_EXT_provoking_vertex
  , glProvokingVertexEXT
  , pattern GL_FIRST_VERTEX_CONVENTION_EXT
  , pattern GL_LAST_VERTEX_CONVENTION_EXT
  , pattern GL_PROVOKING_VERTEX_EXT
  , pattern GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/provoking_vertex.txt GL_EXT_provoking_vertex> extension is available.

gl_EXT_provoking_vertex :: Bool
gl_EXT_provoking_vertex = member "GL_EXT_provoking_vertex" extensions
{-# NOINLINE gl_EXT_provoking_vertex #-}

-- | Usage: @'glProvokingVertexEXT' mode@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glProvokingVertex'.


glProvokingVertexEXT :: MonadIO m => GLenum -> m ()
glProvokingVertexEXT = ffienumIOV glProvokingVertexEXTFunPtr

glProvokingVertexEXTFunPtr :: FunPtr (GLenum -> IO ())
glProvokingVertexEXTFunPtr = unsafePerformIO (getProcAddress "glProvokingVertexEXT")

{-# NOINLINE glProvokingVertexEXTFunPtr #-}

pattern GL_PROVOKING_VERTEX_EXT  :: (Eq a, Num a) => a

pattern GL_PROVOKING_VERTEX_EXT = 0x8E4F

pattern GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION_EXT  :: (Eq a, Num a) => a

pattern GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION_EXT = 0x8E4C