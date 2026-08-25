-- | Native AArch64 code generation for runtime-explicit GRIN.
module Aihc.Arm64
  ( Arm64Error (..),
    compileModule,
    ObservedProgram (..),
    compileObservedFunction,
    compileProgram,
    snapshotSourcePath,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Codegen
  ( Arm64Error (..),
    ObservedProgram (..),
    compileModule,
    compileObservedFunction,
    compileProgram,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Aihc.Native
  ( NativeTarget (AppleArm64),
    nativeTargetTriple,
    snapshotSourcePath,
  )

-- | LLVM target triple for the assembly emitted by this backend.
targetTriple :: String
targetTriple = nativeTargetTriple AppleArm64
