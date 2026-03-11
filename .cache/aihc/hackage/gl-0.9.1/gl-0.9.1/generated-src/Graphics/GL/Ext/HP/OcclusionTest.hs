-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.HP.OcclusionTest (
  -- * Extension Support
    gl_HP_occlusion_test

  -- * GL_HP_occlusion_test
  , pattern GL_OCCLUSION_TEST_HP
  , pattern GL_OCCLUSION_TEST_RESULT_HP
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/HP/occlusion_test.txt GL_HP_occlusion_test> extension is available.

gl_HP_occlusion_test :: Bool
gl_HP_occlusion_test = member "GL_HP_occlusion_test" extensions
{-# NOINLINE gl_HP_occlusion_test #-}

pattern GL_OCCLUSION_TEST_HP  :: (Eq a, Num a) => a

pattern GL_OCCLUSION_TEST_HP = 0x8165

pattern GL_OCCLUSION_TEST_RESULT_HP  :: (Eq a, Num a) => a

pattern GL_OCCLUSION_TEST_RESULT_HP = 0x8166