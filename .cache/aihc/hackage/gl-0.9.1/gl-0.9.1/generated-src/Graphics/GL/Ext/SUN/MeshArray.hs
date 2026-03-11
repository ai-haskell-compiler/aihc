-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SUN.MeshArray (
  -- * Extension Support
    gl_SUN_mesh_array

  -- * GL_SUN_mesh_array
  , glDrawMeshArraysSUN
  , pattern GL_QUAD_MESH_SUN
  , pattern GL_TRIANGLE_MESH_SUN
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SUN/mesh_array.txt GL_SUN_mesh_array> extension is available.

gl_SUN_mesh_array :: Bool
gl_SUN_mesh_array = member "GL_SUN_mesh_array" extensions
{-# NOINLINE gl_SUN_mesh_array #-}

-- | Usage: @'glDrawMeshArraysSUN' mode first count width@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.


glDrawMeshArraysSUN :: MonadIO m => GLenum -> GLint -> GLsizei -> GLsizei -> m ()
glDrawMeshArraysSUN = ffienumintsizeisizeiIOV glDrawMeshArraysSUNFunPtr

glDrawMeshArraysSUNFunPtr :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
glDrawMeshArraysSUNFunPtr = unsafePerformIO (getProcAddress "glDrawMeshArraysSUN")

{-# NOINLINE glDrawMeshArraysSUNFunPtr #-}

pattern GL_QUAD_MESH_SUN  :: (Eq a, Num a) => a

pattern GL_QUAD_MESH_SUN = 0x8614

pattern GL_TRIANGLE_MESH_SUN  :: (Eq a, Num a) => a

pattern GL_TRIANGLE_MESH_SUN = 0x8615