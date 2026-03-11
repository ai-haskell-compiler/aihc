-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexProgram (
  -- * Extension Support
    gl_NV_vertex_program

  -- * GL_NV_vertex_program
  , glAreProgramsResidentNV
  , glBindProgramNV
  , glDeleteProgramsNV
  , glExecuteProgramNV
  , glGenProgramsNV
  , glGetProgramParameterdvNV
  , glGetProgramParameterfvNV
  , glGetProgramStringNV
  , glGetProgramivNV
  , glGetTrackMatrixivNV
  , glGetVertexAttribPointervNV
  , glGetVertexAttribdvNV
  , glGetVertexAttribfvNV
  , glGetVertexAttribivNV
  , glIsProgramNV
  , glLoadProgramNV
  , glProgramParameter4dNV
  , glProgramParameter4dvNV
  , glProgramParameter4fNV
  , glProgramParameter4fvNV
  , glProgramParameters4dvNV
  , glProgramParameters4fvNV
  , glRequestResidentProgramsNV
  , glTrackMatrixNV
  , glVertexAttrib1dNV
  , glVertexAttrib1dvNV
  , glVertexAttrib1fNV
  , glVertexAttrib1fvNV
  , glVertexAttrib1sNV
  , glVertexAttrib1svNV
  , glVertexAttrib2dNV
  , glVertexAttrib2dvNV
  , glVertexAttrib2fNV
  , glVertexAttrib2fvNV
  , glVertexAttrib2sNV
  , glVertexAttrib2svNV
  , glVertexAttrib3dNV
  , glVertexAttrib3dvNV
  , glVertexAttrib3fNV
  , glVertexAttrib3fvNV
  , glVertexAttrib3sNV
  , glVertexAttrib3svNV
  , glVertexAttrib4dNV
  , glVertexAttrib4dvNV
  , glVertexAttrib4fNV
  , glVertexAttrib4fvNV
  , glVertexAttrib4sNV
  , glVertexAttrib4svNV
  , glVertexAttrib4ubNV
  , glVertexAttrib4ubvNV
  , glVertexAttribPointerNV
  , glVertexAttribs1dvNV
  , glVertexAttribs1fvNV
  , glVertexAttribs1svNV
  , glVertexAttribs2dvNV
  , glVertexAttribs2fvNV
  , glVertexAttribs2svNV
  , glVertexAttribs3dvNV
  , glVertexAttribs3fvNV
  , glVertexAttribs3svNV
  , glVertexAttribs4dvNV
  , glVertexAttribs4fvNV
  , glVertexAttribs4svNV
  , glVertexAttribs4ubvNV
  , pattern GL_ATTRIB_ARRAY_POINTER_NV
  , pattern GL_ATTRIB_ARRAY_SIZE_NV
  , pattern GL_ATTRIB_ARRAY_STRIDE_NV
  , pattern GL_ATTRIB_ARRAY_TYPE_NV
  , pattern GL_CURRENT_ATTRIB_NV
  , pattern GL_CURRENT_MATRIX_NV
  , pattern GL_CURRENT_MATRIX_STACK_DEPTH_NV
  , pattern GL_IDENTITY_NV
  , pattern GL_INVERSE_NV
  , pattern GL_INVERSE_TRANSPOSE_NV
  , pattern GL_MAP1_VERTEX_ATTRIB0_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB10_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB11_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB12_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB13_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB14_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB15_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB1_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB2_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB3_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB4_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB5_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB6_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB7_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB8_4_NV
  , pattern GL_MAP1_VERTEX_ATTRIB9_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB0_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB10_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB11_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB12_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB13_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB14_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB15_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB1_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB2_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB3_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB4_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB5_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB6_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB7_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB8_4_NV
  , pattern GL_MAP2_VERTEX_ATTRIB9_4_NV
  , pattern GL_MATRIX0_NV
  , pattern GL_MATRIX1_NV
  , pattern GL_MATRIX2_NV
  , pattern GL_MATRIX3_NV
  , pattern GL_MATRIX4_NV
  , pattern GL_MATRIX5_NV
  , pattern GL_MATRIX6_NV
  , pattern GL_MATRIX7_NV
  , pattern GL_MAX_TRACK_MATRICES_NV
  , pattern GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV
  , pattern GL_MODELVIEW_PROJECTION_NV
  , pattern GL_PROGRAM_ERROR_POSITION_NV
  , pattern GL_PROGRAM_LENGTH_NV
  , pattern GL_PROGRAM_PARAMETER_NV
  , pattern GL_PROGRAM_RESIDENT_NV
  , pattern GL_PROGRAM_STRING_NV
  , pattern GL_PROGRAM_TARGET_NV
  , pattern GL_TRACK_MATRIX_NV
  , pattern GL_TRACK_MATRIX_TRANSFORM_NV
  , pattern GL_TRANSPOSE_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY0_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY10_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY11_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY12_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY13_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY14_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY15_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY1_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY2_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY3_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY4_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY5_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY6_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY7_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY8_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY9_NV
  , pattern GL_VERTEX_PROGRAM_BINDING_NV
  , pattern GL_VERTEX_PROGRAM_NV
  , pattern GL_VERTEX_PROGRAM_POINT_SIZE_NV
  , pattern GL_VERTEX_PROGRAM_TWO_SIDE_NV
  , pattern GL_VERTEX_STATE_PROGRAM_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_program.txt GL_NV_vertex_program> extension is available.

gl_NV_vertex_program :: Bool
gl_NV_vertex_program = member "GL_NV_vertex_program" extensions
{-# NOINLINE gl_NV_vertex_program #-}

-- | Usage: @'glAreProgramsResidentNV' n programs residences@
--
-- The parameter @residences@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @programs@ should be @n@.
--
-- The length of @residences@ should be @n@.


glAreProgramsResidentNV :: MonadIO m => GLsizei -> Ptr GLuint -> Ptr GLboolean -> m GLboolean
glAreProgramsResidentNV = ffisizeiPtruintPtrbooleanIOboolean glAreProgramsResidentNVFunPtr

glAreProgramsResidentNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean)
glAreProgramsResidentNVFunPtr = unsafePerformIO (getProcAddress "glAreProgramsResidentNV")

{-# NOINLINE glAreProgramsResidentNVFunPtr #-}

-- | Usage: @'glBindProgramNV' target id@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindProgramARB'.


glBindProgramNV :: MonadIO m => GLenum -> GLuint -> m ()
glBindProgramNV = ffienumuintIOV glBindProgramNVFunPtr

glBindProgramNVFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBindProgramNVFunPtr = unsafePerformIO (getProcAddress "glBindProgramNV")

{-# NOINLINE glBindProgramNVFunPtr #-}

-- | Usage: @'glDeleteProgramsNV' n programs@
--
-- The length of @programs@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteProgramsARB'.


glDeleteProgramsNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteProgramsNV = ffisizeiPtruintIOV glDeleteProgramsNVFunPtr

glDeleteProgramsNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteProgramsNVFunPtr = unsafePerformIO (getProcAddress "glDeleteProgramsNV")

{-# NOINLINE glDeleteProgramsNVFunPtr #-}

-- | Usage: @'glExecuteProgramNV' target id params@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The length of @params@ should be @4@.


glExecuteProgramNV :: MonadIO m => GLenum -> GLuint -> Ptr GLfloat -> m ()
glExecuteProgramNV = ffienumuintPtrfloatIOV glExecuteProgramNVFunPtr

glExecuteProgramNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
glExecuteProgramNVFunPtr = unsafePerformIO (getProcAddress "glExecuteProgramNV")

{-# NOINLINE glExecuteProgramNVFunPtr #-}

-- | Usage: @'glGenProgramsNV' n programs@
--
-- The length of @programs@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenProgramsARB'.


glGenProgramsNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenProgramsNV = ffisizeiPtruintIOV glGenProgramsNVFunPtr

glGenProgramsNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenProgramsNVFunPtr = unsafePerformIO (getProcAddress "glGenProgramsNV")

{-# NOINLINE glGenProgramsNVFunPtr #-}

-- | Usage: @'glGetProgramParameterdvNV' target index pname params@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The length of @params@ should be @4@.


glGetProgramParameterdvNV :: MonadIO m => GLenum -> GLuint -> GLenum -> Ptr GLdouble -> m ()
glGetProgramParameterdvNV = ffienumuintenumPtrdoubleIOV glGetProgramParameterdvNVFunPtr

glGetProgramParameterdvNVFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
glGetProgramParameterdvNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramParameterdvNV")

{-# NOINLINE glGetProgramParameterdvNVFunPtr #-}

-- | Usage: @'glGetProgramParameterfvNV' target index pname params@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The length of @params@ should be @4@.


glGetProgramParameterfvNV :: MonadIO m => GLenum -> GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetProgramParameterfvNV = ffienumuintenumPtrfloatIOV glGetProgramParameterfvNVFunPtr

glGetProgramParameterfvNVFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetProgramParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramParameterfvNV")

{-# NOINLINE glGetProgramParameterfvNVFunPtr #-}

-- | Usage: @'glGetProgramStringNV' id pname program@
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The parameter @program@ is a @ProgramCharacterNV@.
--
-- The length of @program@ should be @COMPSIZE(id,pname)@.


glGetProgramStringNV :: MonadIO m => GLuint -> GLenum -> Ptr GLubyte -> m ()
glGetProgramStringNV = ffiuintenumPtrubyteIOV glGetProgramStringNVFunPtr

glGetProgramStringNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLubyte -> IO ())
glGetProgramStringNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramStringNV")

{-# NOINLINE glGetProgramStringNVFunPtr #-}

-- | Usage: @'glGetProgramivNV' id pname params@
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The length of @params@ should be @4@.


glGetProgramivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetProgramivNV = ffiuintenumPtrintIOV glGetProgramivNVFunPtr

glGetProgramivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetProgramivNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramivNV")

{-# NOINLINE glGetProgramivNVFunPtr #-}

-- | Usage: @'glGetTrackMatrixivNV' target address pname params@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The length of @params@ should be @1@.


glGetTrackMatrixivNV :: MonadIO m => GLenum -> GLuint -> GLenum -> Ptr GLint -> m ()
glGetTrackMatrixivNV = ffienumuintenumPtrintIOV glGetTrackMatrixivNVFunPtr

glGetTrackMatrixivNVFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
glGetTrackMatrixivNVFunPtr = unsafePerformIO (getProcAddress "glGetTrackMatrixivNV")

{-# NOINLINE glGetTrackMatrixivNVFunPtr #-}

-- | Usage: @'glGetVertexAttribPointervNV' index pname pointer@
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The length of @pointer@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetVertexAttribPointerv'.


glGetVertexAttribPointervNV :: MonadIO m => GLuint -> GLenum -> Ptr (Ptr ()) -> m ()
glGetVertexAttribPointervNV = ffiuintenumPtrPtrVIOV glGetVertexAttribPointervNVFunPtr

glGetVertexAttribPointervNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr (Ptr ()) -> IO ())
glGetVertexAttribPointervNVFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribPointervNV")

{-# NOINLINE glGetVertexAttribPointervNVFunPtr #-}

-- | Usage: @'glGetVertexAttribdvNV' index pname params@
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The length of @params@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetVertexAttribdv'.


glGetVertexAttribdvNV :: MonadIO m => GLuint -> GLenum -> Ptr GLdouble -> m ()
glGetVertexAttribdvNV = ffiuintenumPtrdoubleIOV glGetVertexAttribdvNVFunPtr

glGetVertexAttribdvNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
glGetVertexAttribdvNVFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribdvNV")

{-# NOINLINE glGetVertexAttribdvNVFunPtr #-}

-- | Usage: @'glGetVertexAttribfvNV' index pname params@
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The length of @params@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetVertexAttribfv'.


glGetVertexAttribfvNV :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetVertexAttribfvNV = ffiuintenumPtrfloatIOV glGetVertexAttribfvNVFunPtr

glGetVertexAttribfvNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetVertexAttribfvNVFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribfvNV")

{-# NOINLINE glGetVertexAttribfvNVFunPtr #-}

-- | Usage: @'glGetVertexAttribivNV' index pname params@
--
-- The parameter @pname@ is a @VertexAttribEnumNV@.
--
-- The length of @params@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetVertexAttribiv'.


glGetVertexAttribivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetVertexAttribivNV = ffiuintenumPtrintIOV glGetVertexAttribivNVFunPtr

glGetVertexAttribivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVertexAttribivNVFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribivNV")

{-# NOINLINE glGetVertexAttribivNVFunPtr #-}

-- | Usage: @'glIsProgramNV' id@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsProgramARB'.


glIsProgramNV :: MonadIO m => GLuint -> m GLboolean
glIsProgramNV = ffiuintIOboolean glIsProgramNVFunPtr

glIsProgramNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsProgramNVFunPtr = unsafePerformIO (getProcAddress "glIsProgramNV")

{-# NOINLINE glIsProgramNVFunPtr #-}

-- | Usage: @'glLoadProgramNV' target id len program@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The length of @program@ should be @len@.


glLoadProgramNV :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLubyte -> m ()
glLoadProgramNV = ffienumuintsizeiPtrubyteIOV glLoadProgramNVFunPtr

glLoadProgramNVFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLubyte -> IO ())
glLoadProgramNVFunPtr = unsafePerformIO (getProcAddress "glLoadProgramNV")

{-# NOINLINE glLoadProgramNVFunPtr #-}

-- | Usage: @'glProgramParameter4dNV' target index x y z w@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glProgramParameter4dvNV'.


glProgramParameter4dNV :: MonadIO m => GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glProgramParameter4dNV = ffienumuintdoubledoubledoubledoubleIOV glProgramParameter4dNVFunPtr

glProgramParameter4dNVFunPtr :: FunPtr (GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glProgramParameter4dNVFunPtr = unsafePerformIO (getProcAddress "glProgramParameter4dNV")

{-# NOINLINE glProgramParameter4dNVFunPtr #-}

-- | Usage: @'glProgramParameter4dvNV' target index v@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The length of @v@ should be @4@.


glProgramParameter4dvNV :: MonadIO m => GLenum -> GLuint -> Ptr GLdouble -> m ()
glProgramParameter4dvNV = ffienumuintPtrdoubleIOV glProgramParameter4dvNVFunPtr

glProgramParameter4dvNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
glProgramParameter4dvNVFunPtr = unsafePerformIO (getProcAddress "glProgramParameter4dvNV")

{-# NOINLINE glProgramParameter4dvNVFunPtr #-}

-- | Usage: @'glProgramParameter4fNV' target index x y z w@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glProgramParameter4fvNV'.


glProgramParameter4fNV :: MonadIO m => GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glProgramParameter4fNV = ffienumuintfloatfloatfloatfloatIOV glProgramParameter4fNVFunPtr

glProgramParameter4fNVFunPtr :: FunPtr (GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glProgramParameter4fNVFunPtr = unsafePerformIO (getProcAddress "glProgramParameter4fNV")

{-# NOINLINE glProgramParameter4fNVFunPtr #-}

-- | Usage: @'glProgramParameter4fvNV' target index v@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The length of @v@ should be @4@.


glProgramParameter4fvNV :: MonadIO m => GLenum -> GLuint -> Ptr GLfloat -> m ()
glProgramParameter4fvNV = ffienumuintPtrfloatIOV glProgramParameter4fvNVFunPtr

glProgramParameter4fvNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
glProgramParameter4fvNVFunPtr = unsafePerformIO (getProcAddress "glProgramParameter4fvNV")

{-# NOINLINE glProgramParameter4fvNVFunPtr #-}

-- | Usage: @'glProgramParameters4dvNV' target index count v@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The length of @v@ should be @count*4@.


glProgramParameters4dvNV :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLdouble -> m ()
glProgramParameters4dvNV = ffienumuintsizeiPtrdoubleIOV glProgramParameters4dvNVFunPtr

glProgramParameters4dvNVFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLdouble -> IO ())
glProgramParameters4dvNVFunPtr = unsafePerformIO (getProcAddress "glProgramParameters4dvNV")

{-# NOINLINE glProgramParameters4dvNVFunPtr #-}

-- | Usage: @'glProgramParameters4fvNV' target index count v@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The length of @v@ should be @count*4@.


glProgramParameters4fvNV :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glProgramParameters4fvNV = ffienumuintsizeiPtrfloatIOV glProgramParameters4fvNVFunPtr

glProgramParameters4fvNVFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glProgramParameters4fvNVFunPtr = unsafePerformIO (getProcAddress "glProgramParameters4fvNV")

{-# NOINLINE glProgramParameters4fvNVFunPtr #-}

-- | Usage: @'glRequestResidentProgramsNV' n programs@
--
-- The length of @programs@ should be @n@.


glRequestResidentProgramsNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glRequestResidentProgramsNV = ffisizeiPtruintIOV glRequestResidentProgramsNVFunPtr

glRequestResidentProgramsNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glRequestResidentProgramsNVFunPtr = unsafePerformIO (getProcAddress "glRequestResidentProgramsNV")

{-# NOINLINE glRequestResidentProgramsNVFunPtr #-}

-- | Usage: @'glTrackMatrixNV' target address matrix transform@
--
-- The parameter @target@ is a @VertexAttribEnumNV@.
--
-- The parameter @matrix@ is a @VertexAttribEnumNV@.
--
-- The parameter @transform@ is a @VertexAttribEnumNV@.


glTrackMatrixNV :: MonadIO m => GLenum -> GLuint -> GLenum -> GLenum -> m ()
glTrackMatrixNV = ffienumuintenumenumIOV glTrackMatrixNVFunPtr

glTrackMatrixNVFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> GLenum -> IO ())
glTrackMatrixNVFunPtr = unsafePerformIO (getProcAddress "glTrackMatrixNV")

{-# NOINLINE glTrackMatrixNVFunPtr #-}

-- | Usage: @'glVertexAttrib1dNV' index x@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib1d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib1dvNV'.


glVertexAttrib1dNV :: MonadIO m => GLuint -> GLdouble -> m ()
glVertexAttrib1dNV = ffiuintdoubleIOV glVertexAttrib1dNVFunPtr

glVertexAttrib1dNVFunPtr :: FunPtr (GLuint -> GLdouble -> IO ())
glVertexAttrib1dNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib1dNV")

{-# NOINLINE glVertexAttrib1dNVFunPtr #-}

-- | Usage: @'glVertexAttrib1dvNV' index v@
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib1dv'.


glVertexAttrib1dvNV :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVertexAttrib1dvNV = ffiuintPtrdoubleIOV glVertexAttrib1dvNVFunPtr

glVertexAttrib1dvNVFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVertexAttrib1dvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib1dvNV")

{-# NOINLINE glVertexAttrib1dvNVFunPtr #-}

-- | Usage: @'glVertexAttrib1fNV' index x@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib1f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib1fvNV'.


glVertexAttrib1fNV :: MonadIO m => GLuint -> GLfloat -> m ()
glVertexAttrib1fNV = ffiuintfloatIOV glVertexAttrib1fNVFunPtr

glVertexAttrib1fNVFunPtr :: FunPtr (GLuint -> GLfloat -> IO ())
glVertexAttrib1fNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib1fNV")

{-# NOINLINE glVertexAttrib1fNVFunPtr #-}

-- | Usage: @'glVertexAttrib1fvNV' index v@
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib1fv'.


glVertexAttrib1fvNV :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glVertexAttrib1fvNV = ffiuintPtrfloatIOV glVertexAttrib1fvNVFunPtr

glVertexAttrib1fvNVFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glVertexAttrib1fvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib1fvNV")

{-# NOINLINE glVertexAttrib1fvNVFunPtr #-}

-- | Usage: @'glVertexAttrib1sNV' index x@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib1s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib1svNV'.


glVertexAttrib1sNV :: MonadIO m => GLuint -> GLshort -> m ()
glVertexAttrib1sNV = ffiuintshortIOV glVertexAttrib1sNVFunPtr

glVertexAttrib1sNVFunPtr :: FunPtr (GLuint -> GLshort -> IO ())
glVertexAttrib1sNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib1sNV")

{-# NOINLINE glVertexAttrib1sNVFunPtr #-}

-- | Usage: @'glVertexAttrib1svNV' index v@
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib1sv'.


glVertexAttrib1svNV :: MonadIO m => GLuint -> Ptr GLshort -> m ()
glVertexAttrib1svNV = ffiuintPtrshortIOV glVertexAttrib1svNVFunPtr

glVertexAttrib1svNVFunPtr :: FunPtr (GLuint -> Ptr GLshort -> IO ())
glVertexAttrib1svNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib1svNV")

{-# NOINLINE glVertexAttrib1svNVFunPtr #-}

-- | Usage: @'glVertexAttrib2dNV' index x y@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib2d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib2dvNV'.


glVertexAttrib2dNV :: MonadIO m => GLuint -> GLdouble -> GLdouble -> m ()
glVertexAttrib2dNV = ffiuintdoubledoubleIOV glVertexAttrib2dNVFunPtr

glVertexAttrib2dNVFunPtr :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
glVertexAttrib2dNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib2dNV")

{-# NOINLINE glVertexAttrib2dNVFunPtr #-}

-- | Usage: @'glVertexAttrib2dvNV' index v@
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib2dv'.


glVertexAttrib2dvNV :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVertexAttrib2dvNV = ffiuintPtrdoubleIOV glVertexAttrib2dvNVFunPtr

glVertexAttrib2dvNVFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVertexAttrib2dvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib2dvNV")

{-# NOINLINE glVertexAttrib2dvNVFunPtr #-}

-- | Usage: @'glVertexAttrib2fNV' index x y@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib2f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib2fvNV'.


glVertexAttrib2fNV :: MonadIO m => GLuint -> GLfloat -> GLfloat -> m ()
glVertexAttrib2fNV = ffiuintfloatfloatIOV glVertexAttrib2fNVFunPtr

glVertexAttrib2fNVFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
glVertexAttrib2fNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib2fNV")

{-# NOINLINE glVertexAttrib2fNVFunPtr #-}

-- | Usage: @'glVertexAttrib2fvNV' index v@
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib2fv'.


glVertexAttrib2fvNV :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glVertexAttrib2fvNV = ffiuintPtrfloatIOV glVertexAttrib2fvNVFunPtr

glVertexAttrib2fvNVFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glVertexAttrib2fvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib2fvNV")

{-# NOINLINE glVertexAttrib2fvNVFunPtr #-}

-- | Usage: @'glVertexAttrib2sNV' index x y@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib2s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib2svNV'.


glVertexAttrib2sNV :: MonadIO m => GLuint -> GLshort -> GLshort -> m ()
glVertexAttrib2sNV = ffiuintshortshortIOV glVertexAttrib2sNVFunPtr

glVertexAttrib2sNVFunPtr :: FunPtr (GLuint -> GLshort -> GLshort -> IO ())
glVertexAttrib2sNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib2sNV")

{-# NOINLINE glVertexAttrib2sNVFunPtr #-}

-- | Usage: @'glVertexAttrib2svNV' index v@
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib2sv'.


glVertexAttrib2svNV :: MonadIO m => GLuint -> Ptr GLshort -> m ()
glVertexAttrib2svNV = ffiuintPtrshortIOV glVertexAttrib2svNVFunPtr

glVertexAttrib2svNVFunPtr :: FunPtr (GLuint -> Ptr GLshort -> IO ())
glVertexAttrib2svNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib2svNV")

{-# NOINLINE glVertexAttrib2svNVFunPtr #-}

-- | Usage: @'glVertexAttrib3dNV' index x y z@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib3d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib3dvNV'.


glVertexAttrib3dNV :: MonadIO m => GLuint -> GLdouble -> GLdouble -> GLdouble -> m ()
glVertexAttrib3dNV = ffiuintdoubledoubledoubleIOV glVertexAttrib3dNVFunPtr

glVertexAttrib3dNVFunPtr :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
glVertexAttrib3dNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib3dNV")

{-# NOINLINE glVertexAttrib3dNVFunPtr #-}

-- | Usage: @'glVertexAttrib3dvNV' index v@
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib3dv'.


glVertexAttrib3dvNV :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVertexAttrib3dvNV = ffiuintPtrdoubleIOV glVertexAttrib3dvNVFunPtr

glVertexAttrib3dvNVFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVertexAttrib3dvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib3dvNV")

{-# NOINLINE glVertexAttrib3dvNVFunPtr #-}

-- | Usage: @'glVertexAttrib3fNV' index x y z@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib3f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib3fvNV'.


glVertexAttrib3fNV :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> m ()
glVertexAttrib3fNV = ffiuintfloatfloatfloatIOV glVertexAttrib3fNVFunPtr

glVertexAttrib3fNVFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
glVertexAttrib3fNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib3fNV")

{-# NOINLINE glVertexAttrib3fNVFunPtr #-}

-- | Usage: @'glVertexAttrib3fvNV' index v@
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib3fv'.


glVertexAttrib3fvNV :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glVertexAttrib3fvNV = ffiuintPtrfloatIOV glVertexAttrib3fvNVFunPtr

glVertexAttrib3fvNVFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glVertexAttrib3fvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib3fvNV")

{-# NOINLINE glVertexAttrib3fvNVFunPtr #-}

-- | Usage: @'glVertexAttrib3sNV' index x y z@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib3s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib3svNV'.


glVertexAttrib3sNV :: MonadIO m => GLuint -> GLshort -> GLshort -> GLshort -> m ()
glVertexAttrib3sNV = ffiuintshortshortshortIOV glVertexAttrib3sNVFunPtr

glVertexAttrib3sNVFunPtr :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ())
glVertexAttrib3sNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib3sNV")

{-# NOINLINE glVertexAttrib3sNVFunPtr #-}

-- | Usage: @'glVertexAttrib3svNV' index v@
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib3sv'.


glVertexAttrib3svNV :: MonadIO m => GLuint -> Ptr GLshort -> m ()
glVertexAttrib3svNV = ffiuintPtrshortIOV glVertexAttrib3svNVFunPtr

glVertexAttrib3svNVFunPtr :: FunPtr (GLuint -> Ptr GLshort -> IO ())
glVertexAttrib3svNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib3svNV")

{-# NOINLINE glVertexAttrib3svNVFunPtr #-}

-- | Usage: @'glVertexAttrib4dNV' index x y z w@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib4d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib4dvNV'.


glVertexAttrib4dNV :: MonadIO m => GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glVertexAttrib4dNV = ffiuintdoubledoubledoubledoubleIOV glVertexAttrib4dNVFunPtr

glVertexAttrib4dNVFunPtr :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glVertexAttrib4dNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4dNV")

{-# NOINLINE glVertexAttrib4dNVFunPtr #-}

-- | Usage: @'glVertexAttrib4dvNV' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib4dv'.


glVertexAttrib4dvNV :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVertexAttrib4dvNV = ffiuintPtrdoubleIOV glVertexAttrib4dvNVFunPtr

glVertexAttrib4dvNVFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVertexAttrib4dvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4dvNV")

{-# NOINLINE glVertexAttrib4dvNVFunPtr #-}

-- | Usage: @'glVertexAttrib4fNV' index x y z w@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib4f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib4fvNV'.


glVertexAttrib4fNV :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glVertexAttrib4fNV = ffiuintfloatfloatfloatfloatIOV glVertexAttrib4fNVFunPtr

glVertexAttrib4fNVFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glVertexAttrib4fNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4fNV")

{-# NOINLINE glVertexAttrib4fNVFunPtr #-}

-- | Usage: @'glVertexAttrib4fvNV' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib4fv'.


glVertexAttrib4fvNV :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glVertexAttrib4fvNV = ffiuintPtrfloatIOV glVertexAttrib4fvNVFunPtr

glVertexAttrib4fvNVFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glVertexAttrib4fvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4fvNV")

{-# NOINLINE glVertexAttrib4fvNVFunPtr #-}

-- | Usage: @'glVertexAttrib4sNV' index x y z w@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib4s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib4svNV'.


glVertexAttrib4sNV :: MonadIO m => GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> m ()
glVertexAttrib4sNV = ffiuintshortshortshortshortIOV glVertexAttrib4sNVFunPtr

glVertexAttrib4sNVFunPtr :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
glVertexAttrib4sNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4sNV")

{-# NOINLINE glVertexAttrib4sNVFunPtr #-}

-- | Usage: @'glVertexAttrib4svNV' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib4sv'.


glVertexAttrib4svNV :: MonadIO m => GLuint -> Ptr GLshort -> m ()
glVertexAttrib4svNV = ffiuintPtrshortIOV glVertexAttrib4svNVFunPtr

glVertexAttrib4svNVFunPtr :: FunPtr (GLuint -> Ptr GLshort -> IO ())
glVertexAttrib4svNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4svNV")

{-# NOINLINE glVertexAttrib4svNVFunPtr #-}

-- | Usage: @'glVertexAttrib4ubNV' index x y z w@
--
-- The parameter @x@ is a @ColorUB@.
--
-- The parameter @y@ is a @ColorUB@.
--
-- The parameter @z@ is a @ColorUB@.
--
-- The parameter @w@ is a @ColorUB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib4Nub'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram.glVertexAttrib4ubvNV'.


glVertexAttrib4ubNV :: MonadIO m => GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> m ()
glVertexAttrib4ubNV = ffiuintubyteubyteubyteubyteIOV glVertexAttrib4ubNVFunPtr

glVertexAttrib4ubNVFunPtr :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
glVertexAttrib4ubNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4ubNV")

{-# NOINLINE glVertexAttrib4ubNVFunPtr #-}

-- | Usage: @'glVertexAttrib4ubvNV' index v@
--
-- The parameter @v@ is a @ColorUB@.
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttrib4Nubv'.


glVertexAttrib4ubvNV :: MonadIO m => GLuint -> Ptr GLubyte -> m ()
glVertexAttrib4ubvNV = ffiuintPtrubyteIOV glVertexAttrib4ubvNVFunPtr

glVertexAttrib4ubvNVFunPtr :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
glVertexAttrib4ubvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4ubvNV")

{-# NOINLINE glVertexAttrib4ubvNVFunPtr #-}

-- | Usage: @'glVertexAttribPointerNV' index fsize type stride pointer@
--
-- The parameter @type@ is a @VertexAttribEnumNV@.
--
-- The length of @pointer@ should be @COMPSIZE(fsize,type,stride)@.


glVertexAttribPointerNV :: MonadIO m => GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glVertexAttribPointerNV = ffiuintintenumsizeiPtrVIOV glVertexAttribPointerNVFunPtr

glVertexAttribPointerNVFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glVertexAttribPointerNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribPointerNV")

{-# NOINLINE glVertexAttribPointerNVFunPtr #-}

-- | Usage: @'glVertexAttribs1dvNV' index count v@
--
-- The length of @v@ should be @count@.


glVertexAttribs1dvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLdouble -> m ()
glVertexAttribs1dvNV = ffiuintsizeiPtrdoubleIOV glVertexAttribs1dvNVFunPtr

glVertexAttribs1dvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
glVertexAttribs1dvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs1dvNV")

{-# NOINLINE glVertexAttribs1dvNVFunPtr #-}

-- | Usage: @'glVertexAttribs1fvNV' index count v@
--
-- The length of @v@ should be @count@.


glVertexAttribs1fvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glVertexAttribs1fvNV = ffiuintsizeiPtrfloatIOV glVertexAttribs1fvNVFunPtr

glVertexAttribs1fvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glVertexAttribs1fvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs1fvNV")

{-# NOINLINE glVertexAttribs1fvNVFunPtr #-}

-- | Usage: @'glVertexAttribs1svNV' index count v@
--
-- The length of @v@ should be @count@.


glVertexAttribs1svNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLshort -> m ()
glVertexAttribs1svNV = ffiuintsizeiPtrshortIOV glVertexAttribs1svNVFunPtr

glVertexAttribs1svNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
glVertexAttribs1svNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs1svNV")

{-# NOINLINE glVertexAttribs1svNVFunPtr #-}

-- | Usage: @'glVertexAttribs2dvNV' index count v@
--
-- The length of @v@ should be @count*2@.


glVertexAttribs2dvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLdouble -> m ()
glVertexAttribs2dvNV = ffiuintsizeiPtrdoubleIOV glVertexAttribs2dvNVFunPtr

glVertexAttribs2dvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
glVertexAttribs2dvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs2dvNV")

{-# NOINLINE glVertexAttribs2dvNVFunPtr #-}

-- | Usage: @'glVertexAttribs2fvNV' index count v@
--
-- The length of @v@ should be @count*2@.


glVertexAttribs2fvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glVertexAttribs2fvNV = ffiuintsizeiPtrfloatIOV glVertexAttribs2fvNVFunPtr

glVertexAttribs2fvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glVertexAttribs2fvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs2fvNV")

{-# NOINLINE glVertexAttribs2fvNVFunPtr #-}

-- | Usage: @'glVertexAttribs2svNV' index count v@
--
-- The length of @v@ should be @count*2@.


glVertexAttribs2svNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLshort -> m ()
glVertexAttribs2svNV = ffiuintsizeiPtrshortIOV glVertexAttribs2svNVFunPtr

glVertexAttribs2svNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
glVertexAttribs2svNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs2svNV")

{-# NOINLINE glVertexAttribs2svNVFunPtr #-}

-- | Usage: @'glVertexAttribs3dvNV' index count v@
--
-- The length of @v@ should be @count*3@.


glVertexAttribs3dvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLdouble -> m ()
glVertexAttribs3dvNV = ffiuintsizeiPtrdoubleIOV glVertexAttribs3dvNVFunPtr

glVertexAttribs3dvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
glVertexAttribs3dvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs3dvNV")

{-# NOINLINE glVertexAttribs3dvNVFunPtr #-}

-- | Usage: @'glVertexAttribs3fvNV' index count v@
--
-- The length of @v@ should be @count*3@.


glVertexAttribs3fvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glVertexAttribs3fvNV = ffiuintsizeiPtrfloatIOV glVertexAttribs3fvNVFunPtr

glVertexAttribs3fvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glVertexAttribs3fvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs3fvNV")

{-# NOINLINE glVertexAttribs3fvNVFunPtr #-}

-- | Usage: @'glVertexAttribs3svNV' index count v@
--
-- The length of @v@ should be @count*3@.


glVertexAttribs3svNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLshort -> m ()
glVertexAttribs3svNV = ffiuintsizeiPtrshortIOV glVertexAttribs3svNVFunPtr

glVertexAttribs3svNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
glVertexAttribs3svNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs3svNV")

{-# NOINLINE glVertexAttribs3svNVFunPtr #-}

-- | Usage: @'glVertexAttribs4dvNV' index count v@
--
-- The length of @v@ should be @count*4@.


glVertexAttribs4dvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLdouble -> m ()
glVertexAttribs4dvNV = ffiuintsizeiPtrdoubleIOV glVertexAttribs4dvNVFunPtr

glVertexAttribs4dvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
glVertexAttribs4dvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs4dvNV")

{-# NOINLINE glVertexAttribs4dvNVFunPtr #-}

-- | Usage: @'glVertexAttribs4fvNV' index count v@
--
-- The length of @v@ should be @count*4@.


glVertexAttribs4fvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glVertexAttribs4fvNV = ffiuintsizeiPtrfloatIOV glVertexAttribs4fvNVFunPtr

glVertexAttribs4fvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glVertexAttribs4fvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs4fvNV")

{-# NOINLINE glVertexAttribs4fvNVFunPtr #-}

-- | Usage: @'glVertexAttribs4svNV' index count v@
--
-- The length of @v@ should be @count*4@.


glVertexAttribs4svNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLshort -> m ()
glVertexAttribs4svNV = ffiuintsizeiPtrshortIOV glVertexAttribs4svNVFunPtr

glVertexAttribs4svNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
glVertexAttribs4svNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs4svNV")

{-# NOINLINE glVertexAttribs4svNVFunPtr #-}

-- | Usage: @'glVertexAttribs4ubvNV' index count v@
--
-- The parameter @v@ is a @ColorUB@.
--
-- The length of @v@ should be @count*4@.


glVertexAttribs4ubvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLubyte -> m ()
glVertexAttribs4ubvNV = ffiuintsizeiPtrubyteIOV glVertexAttribs4ubvNVFunPtr

glVertexAttribs4ubvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> IO ())
glVertexAttribs4ubvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs4ubvNV")

{-# NOINLINE glVertexAttribs4ubvNVFunPtr #-}

pattern GL_ATTRIB_ARRAY_POINTER_NV  :: (Eq a, Num a) => a

pattern GL_ATTRIB_ARRAY_POINTER_NV = 0x8645

pattern GL_ATTRIB_ARRAY_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_ATTRIB_ARRAY_SIZE_NV = 0x8623

pattern GL_ATTRIB_ARRAY_STRIDE_NV  :: (Eq a, Num a) => a

pattern GL_ATTRIB_ARRAY_STRIDE_NV = 0x8624

pattern GL_ATTRIB_ARRAY_TYPE_NV  :: (Eq a, Num a) => a

pattern GL_ATTRIB_ARRAY_TYPE_NV = 0x8625

pattern GL_CURRENT_ATTRIB_NV  :: (Eq a, Num a) => a

pattern GL_CURRENT_ATTRIB_NV = 0x8626

pattern GL_CURRENT_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_CURRENT_MATRIX_NV = 0x8641

pattern GL_CURRENT_MATRIX_STACK_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_CURRENT_MATRIX_STACK_DEPTH_NV = 0x8640

pattern GL_IDENTITY_NV  :: (Eq a, Num a) => a

pattern GL_IDENTITY_NV = 0x862A

pattern GL_INVERSE_NV  :: (Eq a, Num a) => a

pattern GL_INVERSE_NV = 0x862B

pattern GL_INVERSE_TRANSPOSE_NV  :: (Eq a, Num a) => a

pattern GL_INVERSE_TRANSPOSE_NV = 0x862D

pattern GL_MAP1_VERTEX_ATTRIB0_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB0_4_NV = 0x8660

pattern GL_MAP1_VERTEX_ATTRIB10_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB10_4_NV = 0x866A

pattern GL_MAP1_VERTEX_ATTRIB11_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB11_4_NV = 0x866B

pattern GL_MAP1_VERTEX_ATTRIB12_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB12_4_NV = 0x866C

pattern GL_MAP1_VERTEX_ATTRIB13_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB13_4_NV = 0x866D

pattern GL_MAP1_VERTEX_ATTRIB14_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB14_4_NV = 0x866E

pattern GL_MAP1_VERTEX_ATTRIB15_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB15_4_NV = 0x866F

pattern GL_MAP1_VERTEX_ATTRIB1_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB1_4_NV = 0x8661

pattern GL_MAP1_VERTEX_ATTRIB2_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB2_4_NV = 0x8662

pattern GL_MAP1_VERTEX_ATTRIB3_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB3_4_NV = 0x8663

pattern GL_MAP1_VERTEX_ATTRIB4_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB4_4_NV = 0x8664

pattern GL_MAP1_VERTEX_ATTRIB5_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB5_4_NV = 0x8665

pattern GL_MAP1_VERTEX_ATTRIB6_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB6_4_NV = 0x8666

pattern GL_MAP1_VERTEX_ATTRIB7_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB7_4_NV = 0x8667

pattern GL_MAP1_VERTEX_ATTRIB8_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB8_4_NV = 0x8668

pattern GL_MAP1_VERTEX_ATTRIB9_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP1_VERTEX_ATTRIB9_4_NV = 0x8669

pattern GL_MAP2_VERTEX_ATTRIB0_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB0_4_NV = 0x8670

pattern GL_MAP2_VERTEX_ATTRIB10_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB10_4_NV = 0x867A

pattern GL_MAP2_VERTEX_ATTRIB11_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB11_4_NV = 0x867B

pattern GL_MAP2_VERTEX_ATTRIB12_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB12_4_NV = 0x867C

pattern GL_MAP2_VERTEX_ATTRIB13_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB13_4_NV = 0x867D

pattern GL_MAP2_VERTEX_ATTRIB14_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB14_4_NV = 0x867E

pattern GL_MAP2_VERTEX_ATTRIB15_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB15_4_NV = 0x867F

pattern GL_MAP2_VERTEX_ATTRIB1_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB1_4_NV = 0x8671

pattern GL_MAP2_VERTEX_ATTRIB2_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB2_4_NV = 0x8672

pattern GL_MAP2_VERTEX_ATTRIB3_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB3_4_NV = 0x8673

pattern GL_MAP2_VERTEX_ATTRIB4_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB4_4_NV = 0x8674

pattern GL_MAP2_VERTEX_ATTRIB5_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB5_4_NV = 0x8675

pattern GL_MAP2_VERTEX_ATTRIB6_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB6_4_NV = 0x8676

pattern GL_MAP2_VERTEX_ATTRIB7_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB7_4_NV = 0x8677

pattern GL_MAP2_VERTEX_ATTRIB8_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB8_4_NV = 0x8678

pattern GL_MAP2_VERTEX_ATTRIB9_4_NV  :: (Eq a, Num a) => a

pattern GL_MAP2_VERTEX_ATTRIB9_4_NV = 0x8679

pattern GL_MATRIX0_NV  :: (Eq a, Num a) => a

pattern GL_MATRIX0_NV = 0x8630

pattern GL_MATRIX1_NV  :: (Eq a, Num a) => a

pattern GL_MATRIX1_NV = 0x8631

pattern GL_MATRIX2_NV  :: (Eq a, Num a) => a

pattern GL_MATRIX2_NV = 0x8632

pattern GL_MATRIX3_NV  :: (Eq a, Num a) => a

pattern GL_MATRIX3_NV = 0x8633

pattern GL_MATRIX4_NV  :: (Eq a, Num a) => a

pattern GL_MATRIX4_NV = 0x8634

pattern GL_MATRIX5_NV  :: (Eq a, Num a) => a

pattern GL_MATRIX5_NV = 0x8635

pattern GL_MATRIX6_NV  :: (Eq a, Num a) => a

pattern GL_MATRIX6_NV = 0x8636

pattern GL_MATRIX7_NV  :: (Eq a, Num a) => a

pattern GL_MATRIX7_NV = 0x8637

pattern GL_MAX_TRACK_MATRICES_NV  :: (Eq a, Num a) => a

pattern GL_MAX_TRACK_MATRICES_NV = 0x862F

pattern GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV = 0x862E

pattern GL_MODELVIEW_PROJECTION_NV  :: (Eq a, Num a) => a

pattern GL_MODELVIEW_PROJECTION_NV = 0x8629

pattern GL_PROGRAM_ERROR_POSITION_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_ERROR_POSITION_NV = 0x864B

pattern GL_PROGRAM_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_LENGTH_NV = 0x8627

pattern GL_PROGRAM_PARAMETER_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_PARAMETER_NV = 0x8644

pattern GL_PROGRAM_RESIDENT_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_RESIDENT_NV = 0x8647

pattern GL_PROGRAM_STRING_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_STRING_NV = 0x8628

pattern GL_PROGRAM_TARGET_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_TARGET_NV = 0x8646

pattern GL_TRACK_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_TRACK_MATRIX_NV = 0x8648

pattern GL_TRACK_MATRIX_TRANSFORM_NV  :: (Eq a, Num a) => a

pattern GL_TRACK_MATRIX_TRANSFORM_NV = 0x8649

pattern GL_TRANSPOSE_NV  :: (Eq a, Num a) => a

pattern GL_TRANSPOSE_NV = 0x862C

pattern GL_VERTEX_ATTRIB_ARRAY0_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY0_NV = 0x8650

pattern GL_VERTEX_ATTRIB_ARRAY10_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY10_NV = 0x865A

pattern GL_VERTEX_ATTRIB_ARRAY11_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY11_NV = 0x865B

pattern GL_VERTEX_ATTRIB_ARRAY12_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY12_NV = 0x865C

pattern GL_VERTEX_ATTRIB_ARRAY13_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY13_NV = 0x865D

pattern GL_VERTEX_ATTRIB_ARRAY14_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY14_NV = 0x865E

pattern GL_VERTEX_ATTRIB_ARRAY15_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY15_NV = 0x865F

pattern GL_VERTEX_ATTRIB_ARRAY1_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY1_NV = 0x8651

pattern GL_VERTEX_ATTRIB_ARRAY2_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY2_NV = 0x8652

pattern GL_VERTEX_ATTRIB_ARRAY3_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY3_NV = 0x8653

pattern GL_VERTEX_ATTRIB_ARRAY4_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY4_NV = 0x8654

pattern GL_VERTEX_ATTRIB_ARRAY5_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY5_NV = 0x8655

pattern GL_VERTEX_ATTRIB_ARRAY6_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY6_NV = 0x8656

pattern GL_VERTEX_ATTRIB_ARRAY7_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY7_NV = 0x8657

pattern GL_VERTEX_ATTRIB_ARRAY8_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY8_NV = 0x8658

pattern GL_VERTEX_ATTRIB_ARRAY9_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY9_NV = 0x8659

pattern GL_VERTEX_PROGRAM_BINDING_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_PROGRAM_BINDING_NV = 0x864A

pattern GL_VERTEX_PROGRAM_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_PROGRAM_NV = 0x8620

pattern GL_VERTEX_PROGRAM_POINT_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_PROGRAM_POINT_SIZE_NV = 0x8642

pattern GL_VERTEX_PROGRAM_TWO_SIDE_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_PROGRAM_TWO_SIDE_NV = 0x8643

pattern GL_VERTEX_STATE_PROGRAM_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_STATE_PROGRAM_NV = 0x8621