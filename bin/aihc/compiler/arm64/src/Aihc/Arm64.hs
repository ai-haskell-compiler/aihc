-- | Native AArch64 code generation for runtime-explicit GRIN.
module Aihc.Arm64
  ( Arm64Error (..),
    compileEntry,
    compileEntryObject,
    compileModule,
    compileModuleObject,
    ObservedProgram (..),
    compileObservedFunction,
    snapshotSourcePath,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Assemble (assembleMachO)
import Aihc.Arm64.Codegen
  ( Arm64Error (..),
    ObservedProgram (..),
    compileEntry,
    compileModule,
    compileObservedFunction,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Aihc.Grin.Gc (GcGrinProgram)
import Aihc.Native
  ( NativeTarget (AppleArm64),
    nativeTargetTriple,
    snapshotSourcePath,
  )
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

compileEntryObject :: Either Arm64Error BL.ByteString
compileEntryObject = compileEntry >>= first (Arm64ObjectError . T.pack . show) . assembleMachO

compileModuleObject :: GcGrinProgram -> Either Arm64Error BL.ByteString
compileModuleObject program = compileModule program >>= first (Arm64ObjectError . T.pack . show) . assembleMachO

-- | LLVM target triple for the assembly emitted by this backend.
targetTriple :: String
targetTriple = nativeTargetTriple AppleArm64
