-- | Native AMD64 code generation for runtime-explicit GRIN.
module Aihc.Amd64
  ( Amd64Error (..),
    LinkLayout,
    LinkInterface,
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    compileModule,
    ObservedProgram (..),
    compileObservedFunction,
    compileProgram,
    compileProgramWithDependencies,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    runtimeSourcePath,
    snapshotSourcePath,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Amd64.Codegen
  ( Amd64Error (..),
    ObservedProgram (..),
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
    NativeTarget (LinuxAmd64),
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    nativeTargetTriple,
    runtimeSourcePath,
    snapshotSourcePath,
  )

-- | LLVM target triple for the assembly emitted by this backend.
targetTriple :: String
targetTriple = nativeTargetTriple LinuxAmd64
