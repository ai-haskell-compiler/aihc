-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TessellationProgram5 (
  -- * Extension Support
    gl_NV_tessellation_program5

  -- * GL_NV_tessellation_program5
  , pattern GL_MAX_PROGRAM_PATCH_ATTRIBS_NV
  , pattern GL_TESS_CONTROL_PROGRAM_NV
  , pattern GL_TESS_CONTROL_PROGRAM_PARAMETER_BUFFER_NV
  , pattern GL_TESS_EVALUATION_PROGRAM_NV
  , pattern GL_TESS_EVALUATION_PROGRAM_PARAMETER_BUFFER_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/tessellation_program5.txt GL_NV_tessellation_program5> extension is available.

gl_NV_tessellation_program5 :: Bool
gl_NV_tessellation_program5 = member "GL_NV_tessellation_program5" extensions
{-# NOINLINE gl_NV_tessellation_program5 #-}

pattern GL_MAX_PROGRAM_PATCH_ATTRIBS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_PATCH_ATTRIBS_NV = 0x86D8

pattern GL_TESS_CONTROL_PROGRAM_NV  :: (Eq a, Num a) => a

pattern GL_TESS_CONTROL_PROGRAM_NV = 0x891E

pattern GL_TESS_CONTROL_PROGRAM_PARAMETER_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_TESS_CONTROL_PROGRAM_PARAMETER_BUFFER_NV = 0x8C74

pattern GL_TESS_EVALUATION_PROGRAM_NV  :: (Eq a, Num a) => a

pattern GL_TESS_EVALUATION_PROGRAM_NV = 0x891F

pattern GL_TESS_EVALUATION_PROGRAM_PARAMETER_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_TESS_EVALUATION_PROGRAM_PARAMETER_BUFFER_NV = 0x8C75