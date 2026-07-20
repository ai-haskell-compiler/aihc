-- | Portable C11 code generation with explicit trampoline control flow.
module Aihc.C
  ( CError (..),
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
where

import Aihc.C.Codegen
