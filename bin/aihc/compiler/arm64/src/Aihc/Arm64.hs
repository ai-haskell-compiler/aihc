-- | Native AArch64 code generation for runtime-explicit GRIN.
module Aihc.Arm64
  ( Arm64Error (..),
    compileEntryObject,
    compileModuleObject,
    ObservedProgram (..),
    compileObservedFunction,
    snapshotSourcePath,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Codegen
  ( Arm64Error (..),
    ObservedProgram (..),
    compileEntryObject,
    compileModuleObject,
    compileObservedFunction,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Aihc.Native
  ( NativeTarget (AppleArm64),
    nativeTargetTriple,
    snapshotSourcePath,
  )

-- | LLVM target triple for this backend.
targetTriple :: String
targetTriple = nativeTargetTriple AppleArm64
