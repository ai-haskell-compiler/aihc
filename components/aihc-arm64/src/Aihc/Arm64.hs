-- | Native AArch64 code generation for runtime-explicit GRIN.
module Aihc.Arm64
  ( Arm64Error (..),
    LinkLayout,
    LinkInterface,
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    runtimeSourcePath,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Codegen
  ( Arm64Error (..),
    LinkInterface,
    LinkLayout,
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Paths_aihc_arm64 (getDataFileName)

-- | Locate the C runtime used by generated assembly.
runtimeSourcePath :: IO FilePath
runtimeSourcePath = getDataFileName "runtime/aihc_runtime.c"

-- | LLVM target triple for the assembly emitted by this backend.
targetTriple :: String
targetTriple = "arm64-apple-darwin"
