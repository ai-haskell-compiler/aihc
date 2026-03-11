-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.VertexAttribArrayObject (
  -- * Extension Support
    gl_ATI_vertex_attrib_array_object

  -- * GL_ATI_vertex_attrib_array_object
  , glGetVertexAttribArrayObjectfvATI
  , glGetVertexAttribArrayObjectivATI
  , glVertexAttribArrayObjectATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/vertex_attrib_array_object.txt GL_ATI_vertex_attrib_array_object> extension is available.

gl_ATI_vertex_attrib_array_object :: Bool
gl_ATI_vertex_attrib_array_object = member "GL_ATI_vertex_attrib_array_object" extensions
{-# NOINLINE gl_ATI_vertex_attrib_array_object #-}

-- | Usage: @'glGetVertexAttribArrayObjectfvATI' index pname params@
--
-- The parameter @pname@ is a @ArrayObjectPNameATI@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVertexAttribArrayObjectfvATI :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetVertexAttribArrayObjectfvATI = ffiuintenumPtrfloatIOV glGetVertexAttribArrayObjectfvATIFunPtr

glGetVertexAttribArrayObjectfvATIFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetVertexAttribArrayObjectfvATIFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribArrayObjectfvATI")

{-# NOINLINE glGetVertexAttribArrayObjectfvATIFunPtr #-}

-- | Usage: @'glGetVertexAttribArrayObjectivATI' index pname params@
--
-- The parameter @pname@ is a @ArrayObjectPNameATI@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVertexAttribArrayObjectivATI :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetVertexAttribArrayObjectivATI = ffiuintenumPtrintIOV glGetVertexAttribArrayObjectivATIFunPtr

glGetVertexAttribArrayObjectivATIFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVertexAttribArrayObjectivATIFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribArrayObjectivATI")

{-# NOINLINE glGetVertexAttribArrayObjectivATIFunPtr #-}

-- | Usage: @'glVertexAttribArrayObjectATI' index size type normalized stride buffer offset@
--
-- The parameter @type@ is a @VertexAttribPointerType@.
--
-- The parameter @normalized@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glVertexAttribArrayObjectATI :: MonadIO m => GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLuint -> GLuint -> m ()
glVertexAttribArrayObjectATI = ffiuintintenumbooleansizeiuintuintIOV glVertexAttribArrayObjectATIFunPtr

glVertexAttribArrayObjectATIFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLuint -> GLuint -> IO ())
glVertexAttribArrayObjectATIFunPtr = unsafePerformIO (getProcAddress "glVertexAttribArrayObjectATI")

{-# NOINLINE glVertexAttribArrayObjectATIFunPtr #-}