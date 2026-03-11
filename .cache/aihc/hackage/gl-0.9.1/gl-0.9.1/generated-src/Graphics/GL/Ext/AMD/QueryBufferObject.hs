-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.QueryBufferObject (
  -- * Extension Support
    gl_AMD_query_buffer_object

  -- * GL_AMD_query_buffer_object
  , pattern GL_QUERY_BUFFER_AMD
  , pattern GL_QUERY_BUFFER_BINDING_AMD
  , pattern GL_QUERY_RESULT_NO_WAIT_AMD
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/query_buffer_object.txt GL_AMD_query_buffer_object> extension is available.

gl_AMD_query_buffer_object :: Bool
gl_AMD_query_buffer_object = member "GL_AMD_query_buffer_object" extensions
{-# NOINLINE gl_AMD_query_buffer_object #-}

pattern GL_QUERY_BUFFER_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_BUFFER_AMD = 0x9192

pattern GL_QUERY_BUFFER_BINDING_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_BUFFER_BINDING_AMD = 0x9193

pattern GL_QUERY_RESULT_NO_WAIT_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_RESULT_NO_WAIT_AMD = 0x9194