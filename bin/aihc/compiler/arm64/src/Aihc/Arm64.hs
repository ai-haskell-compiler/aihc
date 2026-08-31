-- | Native AArch64 code generation for runtime-explicit GRIN.
module Aihc.Arm64
  ( Arm64Error (..),
    compileEntryObject,
    compileModuleObject,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Codegen
  ( Arm64Error (..),
    compileEntryObject,
    compileModuleObject,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Aihc.Native
  ( NativeTarget (AppleArm64),
    nativeTargetTriple,
  )

-- | LLVM target triple for this backend.
targetTriple :: String
targetTriple = nativeTargetTriple AppleArm64
