-- | LLVM code generation with guaranteed tail calls.
module Aihc.Llvm
  ( LlvmError (..),
    compileModule,
    compileMainProgramWithDependencies,
    compileProgram,
    compileProgramWithDependencies,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
where

import Aihc.Llvm.Codegen
