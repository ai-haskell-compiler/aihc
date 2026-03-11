-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.IrInstrument1 (
  -- * Extension Support
    gl_SGIX_ir_instrument1

  -- * GL_SGIX_ir_instrument1
  , pattern GL_IR_INSTRUMENT1_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/ir_instrument1.txt GL_SGIX_ir_instrument1> extension is available.

gl_SGIX_ir_instrument1 :: Bool
gl_SGIX_ir_instrument1 = member "GL_SGIX_ir_instrument1" extensions
{-# NOINLINE gl_SGIX_ir_instrument1 #-}

pattern GL_IR_INSTRUMENT1_SGIX  :: (Eq a, Num a) => a

pattern GL_IR_INSTRUMENT1_SGIX = 0x817F