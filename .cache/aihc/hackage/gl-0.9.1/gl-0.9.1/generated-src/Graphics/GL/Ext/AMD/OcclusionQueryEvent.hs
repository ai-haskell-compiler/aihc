-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.OcclusionQueryEvent (
  -- * Extension Support
    gl_AMD_occlusion_query_event

  -- * GL_AMD_occlusion_query_event
  , glQueryObjectParameteruiAMD
  , pattern GL_OCCLUSION_QUERY_EVENT_MASK_AMD
  , pattern GL_QUERY_ALL_EVENT_BITS_AMD
  , pattern GL_QUERY_DEPTH_BOUNDS_FAIL_EVENT_BIT_AMD
  , pattern GL_QUERY_DEPTH_FAIL_EVENT_BIT_AMD
  , pattern GL_QUERY_DEPTH_PASS_EVENT_BIT_AMD
  , pattern GL_QUERY_STENCIL_FAIL_EVENT_BIT_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/occlusion_query_event.txt GL_AMD_occlusion_query_event> extension is available.

gl_AMD_occlusion_query_event :: Bool
gl_AMD_occlusion_query_event = member "GL_AMD_occlusion_query_event" extensions
{-# NOINLINE gl_AMD_occlusion_query_event #-}

-- | Usage: @'glQueryObjectParameteruiAMD' target id pname param@
--
-- The parameter @param@ is a @OcclusionQueryEventMaskAMD@, one of: 'Graphics.GL.Ext.AMD.OcclusionQueryEvent.GL_QUERY_DEPTH_PASS_EVENT_BIT_AMD', 'Graphics.GL.Ext.AMD.OcclusionQueryEvent.GL_QUERY_DEPTH_FAIL_EVENT_BIT_AMD', 'Graphics.GL.Ext.AMD.OcclusionQueryEvent.GL_QUERY_STENCIL_FAIL_EVENT_BIT_AMD', 'Graphics.GL.Ext.AMD.OcclusionQueryEvent.GL_QUERY_DEPTH_BOUNDS_FAIL_EVENT_BIT_AMD', 'Graphics.GL.Ext.AMD.OcclusionQueryEvent.GL_QUERY_ALL_EVENT_BITS_AMD'.


glQueryObjectParameteruiAMD :: MonadIO m => GLenum -> GLuint -> GLenum -> GLuint -> m ()
glQueryObjectParameteruiAMD = ffienumuintenumuintIOV glQueryObjectParameteruiAMDFunPtr

glQueryObjectParameteruiAMDFunPtr :: FunPtr (GLenum -> GLuint -> GLenum -> GLuint -> IO ())
glQueryObjectParameteruiAMDFunPtr = unsafePerformIO (getProcAddress "glQueryObjectParameteruiAMD")

{-# NOINLINE glQueryObjectParameteruiAMDFunPtr #-}

pattern GL_OCCLUSION_QUERY_EVENT_MASK_AMD  :: (Eq a, Num a) => a

pattern GL_OCCLUSION_QUERY_EVENT_MASK_AMD = 0x874F

pattern GL_QUERY_ALL_EVENT_BITS_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_ALL_EVENT_BITS_AMD = 0xFFFFFFFF

pattern GL_QUERY_DEPTH_BOUNDS_FAIL_EVENT_BIT_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_DEPTH_BOUNDS_FAIL_EVENT_BIT_AMD = 0x00000008

pattern GL_QUERY_DEPTH_FAIL_EVENT_BIT_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_DEPTH_FAIL_EVENT_BIT_AMD = 0x00000002

pattern GL_QUERY_DEPTH_PASS_EVENT_BIT_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_DEPTH_PASS_EVENT_BIT_AMD = 0x00000001

pattern GL_QUERY_STENCIL_FAIL_EVENT_BIT_AMD  :: (Eq a, Num a) => a

pattern GL_QUERY_STENCIL_FAIL_EVENT_BIT_AMD = 0x00000004