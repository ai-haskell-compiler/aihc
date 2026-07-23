-- | Direct WebAssembly code generation for runtime-explicit GRIN.
module Aihc.Wasm
  ( WasmError (..),
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    validatePrimitiveNames,
    validateProgramPrimitives,
    wasip3RuntimeSourcePath,
    wasip3WorldPath,
  )
where

import Aihc.Wasm.Codegen
import Paths_aihc_wasm (getDataFileName)

wasip3RuntimeSourcePath :: IO FilePath
wasip3RuntimeSourcePath = getDataFileName "runtime/aihc_wasip3.c"

wasip3WorldPath :: IO FilePath
wasip3WorldPath = getDataFileName "runtime/wit"
