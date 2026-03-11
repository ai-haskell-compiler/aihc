-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.InterleavedElements (
  -- * Extension Support
    gl_AMD_interleaved_elements

  -- * GL_AMD_interleaved_elements
  , glVertexAttribParameteriAMD
  , pattern GL_ALPHA
  , pattern GL_BLUE
  , pattern GL_GREEN
  , pattern GL_RED
  , pattern GL_RG16UI
  , pattern GL_RG8UI
  , pattern GL_RGBA8UI
  , pattern GL_VERTEX_ELEMENT_SWIZZLE_AMD
  , pattern GL_VERTEX_ID_SWIZZLE_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/interleaved_elements.txt GL_AMD_interleaved_elements> extension is available.

gl_AMD_interleaved_elements :: Bool
gl_AMD_interleaved_elements = member "GL_AMD_interleaved_elements" extensions
{-# NOINLINE gl_AMD_interleaved_elements #-}

-- | Usage: @'glVertexAttribParameteriAMD' index pname param@


glVertexAttribParameteriAMD :: MonadIO m => GLuint -> GLenum -> GLint -> m ()
glVertexAttribParameteriAMD = ffiuintenumintIOV glVertexAttribParameteriAMDFunPtr

glVertexAttribParameteriAMDFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
glVertexAttribParameteriAMDFunPtr = unsafePerformIO (getProcAddress "glVertexAttribParameteriAMD")

{-# NOINLINE glVertexAttribParameteriAMDFunPtr #-}

pattern GL_VERTEX_ELEMENT_SWIZZLE_AMD  :: (Eq a, Num a) => a

pattern GL_VERTEX_ELEMENT_SWIZZLE_AMD = 0x91A4

pattern GL_VERTEX_ID_SWIZZLE_AMD  :: (Eq a, Num a) => a

pattern GL_VERTEX_ID_SWIZZLE_AMD = 0x91A5