-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShadingLanguageInclude (
  -- * Extension Support
    gl_ARB_shading_language_include

  -- * GL_ARB_shading_language_include
  , glCompileShaderIncludeARB
  , glDeleteNamedStringARB
  , glGetNamedStringARB
  , glGetNamedStringivARB
  , glIsNamedStringARB
  , glNamedStringARB
  , pattern GL_NAMED_STRING_LENGTH_ARB
  , pattern GL_NAMED_STRING_TYPE_ARB
  , pattern GL_SHADER_INCLUDE_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shading_language_include.txt GL_ARB_shading_language_include> extension is available.

gl_ARB_shading_language_include :: Bool
gl_ARB_shading_language_include = member "GL_ARB_shading_language_include" extensions
{-# NOINLINE gl_ARB_shading_language_include #-}

-- | Usage: @'glCompileShaderIncludeARB' shader count path length@
--
-- The length of @path@ should be @count@.
--
-- The length of @length@ should be @count@.


glCompileShaderIncludeARB :: MonadIO m => GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> m ()
glCompileShaderIncludeARB = ffiuintsizeiPtrPtrcharPtrintIOV glCompileShaderIncludeARBFunPtr

glCompileShaderIncludeARBFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ())
glCompileShaderIncludeARBFunPtr = unsafePerformIO (getProcAddress "glCompileShaderIncludeARB")

{-# NOINLINE glCompileShaderIncludeARBFunPtr #-}

-- | Usage: @'glDeleteNamedStringARB' namelen name@
--
-- The length of @name@ should be @namelen@.


glDeleteNamedStringARB :: MonadIO m => GLint -> Ptr GLchar -> m ()
glDeleteNamedStringARB = ffiintPtrcharIOV glDeleteNamedStringARBFunPtr

glDeleteNamedStringARBFunPtr :: FunPtr (GLint -> Ptr GLchar -> IO ())
glDeleteNamedStringARBFunPtr = unsafePerformIO (getProcAddress "glDeleteNamedStringARB")

{-# NOINLINE glDeleteNamedStringARBFunPtr #-}

-- | Usage: @'glGetNamedStringARB' namelen name bufSize stringlen string@
--
-- The length of @name@ should be @namelen@.
--
-- The length of @stringlen@ should be @1@.
--
-- The length of @string@ should be @bufSize@.


glGetNamedStringARB :: MonadIO m => GLint -> Ptr GLchar -> GLsizei -> Ptr GLint -> Ptr GLchar -> m ()
glGetNamedStringARB = ffiintPtrcharsizeiPtrintPtrcharIOV glGetNamedStringARBFunPtr

glGetNamedStringARBFunPtr :: FunPtr (GLint -> Ptr GLchar -> GLsizei -> Ptr GLint -> Ptr GLchar -> IO ())
glGetNamedStringARBFunPtr = unsafePerformIO (getProcAddress "glGetNamedStringARB")

{-# NOINLINE glGetNamedStringARBFunPtr #-}

-- | Usage: @'glGetNamedStringivARB' namelen name pname params@
--
-- The length of @name@ should be @namelen@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetNamedStringivARB :: MonadIO m => GLint -> Ptr GLchar -> GLenum -> Ptr GLint -> m ()
glGetNamedStringivARB = ffiintPtrcharenumPtrintIOV glGetNamedStringivARBFunPtr

glGetNamedStringivARBFunPtr :: FunPtr (GLint -> Ptr GLchar -> GLenum -> Ptr GLint -> IO ())
glGetNamedStringivARBFunPtr = unsafePerformIO (getProcAddress "glGetNamedStringivARB")

{-# NOINLINE glGetNamedStringivARBFunPtr #-}

-- | Usage: @'glIsNamedStringARB' namelen name@
--
-- The length of @name@ should be @namelen@.


glIsNamedStringARB :: MonadIO m => GLint -> Ptr GLchar -> m GLboolean
glIsNamedStringARB = ffiintPtrcharIOboolean glIsNamedStringARBFunPtr

glIsNamedStringARBFunPtr :: FunPtr (GLint -> Ptr GLchar -> IO GLboolean)
glIsNamedStringARBFunPtr = unsafePerformIO (getProcAddress "glIsNamedStringARB")

{-# NOINLINE glIsNamedStringARBFunPtr #-}

-- | Usage: @'glNamedStringARB' type namelen name stringlen string@
--
-- The length of @name@ should be @namelen@.
--
-- The length of @string@ should be @stringlen@.


glNamedStringARB :: MonadIO m => GLenum -> GLint -> Ptr GLchar -> GLint -> Ptr GLchar -> m ()
glNamedStringARB = ffienumintPtrcharintPtrcharIOV glNamedStringARBFunPtr

glNamedStringARBFunPtr :: FunPtr (GLenum -> GLint -> Ptr GLchar -> GLint -> Ptr GLchar -> IO ())
glNamedStringARBFunPtr = unsafePerformIO (getProcAddress "glNamedStringARB")

{-# NOINLINE glNamedStringARBFunPtr #-}

pattern GL_NAMED_STRING_LENGTH_ARB  :: (Eq a, Num a) => a

pattern GL_NAMED_STRING_LENGTH_ARB = 0x8DE9

pattern GL_NAMED_STRING_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_NAMED_STRING_TYPE_ARB = 0x8DEA

pattern GL_SHADER_INCLUDE_ARB  :: (Eq a, Num a) => a

pattern GL_SHADER_INCLUDE_ARB = 0x8DAE