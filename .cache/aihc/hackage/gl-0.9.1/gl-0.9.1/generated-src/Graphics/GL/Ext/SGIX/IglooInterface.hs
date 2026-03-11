-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.IglooInterface (
  -- * Extension Support
    gl_SGIX_igloo_interface

  -- * GL_SGIX_igloo_interface
  , glIglooInterfaceSGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_SGIX_igloo_interface extension is available.

gl_SGIX_igloo_interface :: Bool
gl_SGIX_igloo_interface = member "GL_SGIX_igloo_interface" extensions
{-# NOINLINE gl_SGIX_igloo_interface #-}

-- | Usage: @'glIglooInterfaceSGIX' pname params@
--
-- The parameter @pname@ is a @IglooFunctionSelectSGIX@.
--
-- The parameter @params@ is a @IglooParameterSGIX@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glIglooInterfaceSGIX :: MonadIO m => GLenum -> Ptr () -> m ()
glIglooInterfaceSGIX = ffienumPtrVIOV glIglooInterfaceSGIXFunPtr

glIglooInterfaceSGIXFunPtr :: FunPtr (GLenum -> Ptr () -> IO ())
glIglooInterfaceSGIXFunPtr = unsafePerformIO (getProcAddress "glIglooInterfaceSGIX")

{-# NOINLINE glIglooInterfaceSGIXFunPtr #-}