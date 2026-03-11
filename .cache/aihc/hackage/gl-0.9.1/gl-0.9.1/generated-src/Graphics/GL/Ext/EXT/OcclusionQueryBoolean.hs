-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.OcclusionQueryBoolean (
  -- * Extension Support
    gl_EXT_occlusion_query_boolean

  -- * GL_EXT_occlusion_query_boolean
  , glBeginQueryEXT
  , glDeleteQueriesEXT
  , glEndQueryEXT
  , glGenQueriesEXT
  , glGetQueryObjectuivEXT
  , glGetQueryivEXT
  , glIsQueryEXT
  , pattern GL_ANY_SAMPLES_PASSED_CONSERVATIVE_EXT
  , pattern GL_ANY_SAMPLES_PASSED_EXT
  , pattern GL_CURRENT_QUERY_EXT
  , pattern GL_QUERY_RESULT_AVAILABLE_EXT
  , pattern GL_QUERY_RESULT_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_occlusion_query_boolean.txt GL_EXT_occlusion_query_boolean> extension is available.

gl_EXT_occlusion_query_boolean :: Bool
gl_EXT_occlusion_query_boolean = member "GL_EXT_occlusion_query_boolean" extensions
{-# NOINLINE gl_EXT_occlusion_query_boolean #-}

pattern GL_ANY_SAMPLES_PASSED_CONSERVATIVE_EXT  :: (Eq a, Num a) => a

pattern GL_ANY_SAMPLES_PASSED_CONSERVATIVE_EXT = 0x8D6A

pattern GL_ANY_SAMPLES_PASSED_EXT  :: (Eq a, Num a) => a

pattern GL_ANY_SAMPLES_PASSED_EXT = 0x8C2F