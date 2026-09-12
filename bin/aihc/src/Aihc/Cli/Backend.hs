-- | The Lir backend of each target. Every target lowers GC-GRIN to Lir and
-- compiles the Lir module with one backend: a direct object writer for
-- Apple ARM64 and Linux AMD64, or a text form that Clang assembles for LLVM
-- and WebAssembly.
module Aihc.Cli.Backend
  ( BackendOutput (..),
    compileLir,
    compileLirWith,
    compileLirTo,
    compileGrinTo,
    lowerTargetFor,
    nativeSourceExtension,
  )
where

import Aihc.Amd64.Lir qualified as Amd64
import Aihc.Arm64.Lir qualified as Arm64
import Aihc.Grin.Gc (GcGrinProgram)
import Aihc.Lir.Lower (LowerTarget, posixTarget64, wasip3Target)
import Aihc.Lir.Lower qualified as Lower
import Aihc.Lir.Syntax (Module)
import Aihc.Llvm.Lir qualified as Llvm
import Aihc.Native (NativeTarget (..))
import Aihc.Wasm.Lir qualified as Wasm
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)

data BackendOutput
  = -- | A finished object file.
    BackendObject !BL.ByteString
  | -- | A source file for the compiler driver of the target.
    BackendSource !Text

-- | The lowering target of a native target.
lowerTargetFor :: NativeTarget -> LowerTarget
lowerTargetFor target =
  case target of
    Wasm32Wasip3 -> wasip3Target
    _ -> posixTarget64

-- | Compile one Lir module for the target, linting it first.
compileLir :: NativeTarget -> Module -> Either String BackendOutput
compileLir = compileLirWith True

-- | Compile one Lir module for the target. The object backends lint the
-- module only when asked to; the text backends always do.
compileLirWith :: Bool -> NativeTarget -> Module -> Either String BackendOutput
compileLirWith lint target lirModule =
  case target of
    AppleArm64 -> either (Left . show) (Right . BackendObject) (Arm64.compileLirObjectWith lint lirModule)
    LinuxAmd64 -> either (Left . show) (Right . BackendObject) (Amd64.compileLirObjectWith lint lirModule)
    Llvm -> either (Left . show) (Right . BackendSource) (Llvm.compileLirModule lirModule)
    Wasm32Wasip3 -> either (Left . show) (Right . BackendSource) (Wasm.compileLirModule lirModule)

-- | Write a native object, or return source for an external compiler.
compileLirTo :: Bool -> NativeTarget -> Module -> FilePath -> IO (Maybe Text)
compileLirTo lint target lirModule path = case target of
  AppleArm64 -> Arm64.writeLirObjectWith lint lirModule path >> pure Nothing
  LinuxAmd64 -> Amd64.writeLirObjectWith lint lirModule path >> pure Nothing
  _ -> do
    output <- either (ioError . userError . ("Lir backend failed: " <>)) pure (compileLirWith lint target lirModule)
    case output of
      BackendObject bytes -> BL.writeFile path bytes >> pure Nothing
      BackendSource source -> pure (Just source)

-- | Use shared incremental conversion for both native object paths.
compileGrinTo :: Bool -> Bool -> NativeTarget -> Maybe FilePath -> GcGrinProgram -> FilePath -> IO (Maybe Text)
compileGrinTo lint checkBounds target dumpPath gcProgram path = case target of
  AppleArm64 -> Arm64.writeGrinObjectWith lint checkBounds dumpPath gcProgram path >> pure Nothing
  LinuxAmd64 -> Amd64.writeGrinObjectWith lint checkBounds dumpPath gcProgram path >> pure Nothing
  _ -> do
    lirModule <- either (ioError . userError . ("Lir generation failed: " <>) . show) pure (Lower.lowerModule (lowerTargetFor target) checkBounds gcProgram)
    compileLirTo lint target lirModule path

-- | The extension of the source kept next to an object. An object target
-- keeps the Lir text.
nativeSourceExtension :: NativeTarget -> String
nativeSourceExtension target =
  case target of
    AppleArm64 -> ".lir"
    LinuxAmd64 -> ".lir"
    Llvm -> ".ll"
    Wasm32Wasip3 -> ".s"
