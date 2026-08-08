-- | Native AArch64 code generation for runtime-explicit GRIN.
module Aihc.Arm64
  ( Arm64Error (..),
    LinkLayout,
    LinkInterface,
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    compileModule,
    ObservedProgram (..),
    compileObservedFunction,
    compileMainProgramWithDependencies,
    compileProgram,
    compileProgramWithDependencies,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    snapshotSourcePath,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Codegen
  ( Arm64Error (..),
    ObservedProgram (..),
    compileMainProgramWithDependencies,
    compileModule,
    compileObservedFunction,
    compileProgram,
    compileProgramWithDependencies,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Aihc.Native
  ( LinkInterface,
    LinkLayout,
    NativeTarget (AppleArm64),
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    nativeTargetTriple,
    snapshotSourcePath,
  )

-- | LLVM target triple for the assembly emitted by this backend.
targetTriple :: String
targetTriple = nativeTargetTriple AppleArm64
