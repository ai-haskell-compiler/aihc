-- | The Lir backend of each target. Every target lowers GC-GRIN to Lir and
-- compiles the Lir module with one backend: a direct object writer for
-- Apple ARM64 and Linux AMD64, or a text form that Clang assembles for LLVM
-- and WebAssembly.
module Aihc.Cli.Backend
  ( BackendOutput (..),
    compileLirWith,
    compileLirTo,
    compileLirObject,
    compileEntryObject,
    compileGrinTo,
    lirModuleDefinesCode,
    lowerTargetFor,
    nativeSourceExtension,
    nativeSourceIsLir,
  )
where

import Aihc.Amd64.Lir qualified as Amd64
import Aihc.Arm64.Lir qualified as Arm64
import Aihc.Grin.Gc (GcGrinProgram)
import Aihc.Lir.Lower (LowerTarget, posixTarget64, wasip3Target)
import Aihc.Lir.Lower qualified as Lower
import Aihc.Lir.Pretty (renderModule)
import Aihc.Lir.Syntax (Item (..), Module (..))
import Aihc.Llvm.Lir qualified as Llvm
import Aihc.Native (NativeTarget (..), backendCompiler, optimizationArgument, runtimeOptimizationLevel)
import Aihc.Wasm.Lir qualified as Wasm
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))
import System.Process (readProcessWithExitCode)

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

-- | Compile one Lir module to an object. An object target writes the object
-- directly. A text target writes the backend source as @name@ with the
-- source extension of the target under @directory@ and lets the compiler
-- driver of the target assemble it.
-- LLVM uses the runtime optimization level for these standalone units,
-- which include the RTS helpers and executable entry code.
compileLirObject :: NativeTarget -> String -> Module -> FilePath -> FilePath -> IO ()
compileLirObject target name lirModule directory object = do
  output <- compileLirTo True target lirModule object
  case output of
    Nothing -> pure ()
    Just source -> do
      let sourcePath = directory </> name <> nativeSourceExtension target
      TIO.writeFile sourcePath source
      (compiler, arguments) <- backendCompiler target
      let optimizationArguments = [optimizationArgument runtimeOptimizationLevel | target == Llvm]
      (exitCode, _stdout, stderr) <- readProcessWithExitCode compiler (arguments <> optimizationArguments <> ["-c", sourcePath, "-o", object]) ""
      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure _ -> ioError (userError (compiler <> " failed (" <> show exitCode <> "): " <> stderr))

-- | Whether a Lir module has anything to put in an object. A unit that holds
-- only constants, such as @aihc_constants.lir@, is there to be included by
-- the others and produces no object.
lirModuleDefinesCode :: Module -> Bool
lirModuleDefinesCode lirModule = any definesCode (moduleItems lirModule)
  where
    definesCode item =
      case item of
        ItemFunction _ -> True
        ItemGlobal _ -> True
        ItemData _ -> True
        ItemExternFunction _ -> False
        ItemExternData _ -> False
        ItemConstant _ -> False
        ItemInclude _ -> False

-- | Compile the entry unit of an executable to @object@. The entry starts
-- the runtime and enters the program; the entry of every executable is the
-- same, so it is generated rather than read from a source.
compileEntryObject :: NativeTarget -> FilePath -> FilePath -> IO ()
compileEntryObject target directory object = do
  entryModule <- either (ioError . userError . ("Lir entry generation failed: " <>) . show) pure (Lower.lowerEntry (lowerTargetFor target))
  compileLirObject target (takeBaseName object) entryModule directory object

-- | Use shared incremental conversion for both native object paths. The Lir
-- of the module is written to @lirPath@ when one is given: an object
-- backend writes each item as conversion produces it, and a source backend
-- writes the module it lowered.
compileGrinTo :: Bool -> Bool -> NativeTarget -> Maybe FilePath -> GcGrinProgram -> FilePath -> IO (Maybe Text)
compileGrinTo lint checkBounds target lirPath gcProgram path = case target of
  AppleArm64 -> Arm64.writeGrinObjectWith lint checkBounds lirPath gcProgram path >> pure Nothing
  LinuxAmd64 -> Amd64.writeGrinObjectWith lint checkBounds lirPath gcProgram path >> pure Nothing
  _ -> do
    lirModule <- either (ioError . userError . ("Lir generation failed: " <>) . show) pure (Lower.lowerModule (lowerTargetFor target) checkBounds gcProgram)
    mapM_ (\dump -> TIO.writeFile dump (renderLirModule lirModule)) lirPath
    compileLirTo lint target lirModule path

-- | The Lir text of a module, as the object backends dump it: one item to
-- a line group, with a final newline.
renderLirModule :: Module -> Text
renderLirModule lirModule =
  foldMap (\item -> renderModule (Module [item]) <> "\n") (moduleItems lirModule)

-- | The extension of the source kept next to an object. An object target
-- keeps the Lir text.
nativeSourceExtension :: NativeTarget -> String
nativeSourceExtension target =
  case target of
    AppleArm64 -> ".lir"
    LinuxAmd64 -> ".lir"
    Llvm -> ".ll"
    Wasm32Wasip3 -> ".s"

-- | Whether the source kept beside the object of a target is the Lir text.
-- An object backend writes the object itself, so Lir is the last form of
-- the module there is to keep.
nativeSourceIsLir :: NativeTarget -> Bool
nativeSourceIsLir target =
  case target of
    AppleArm64 -> True
    LinuxAmd64 -> True
    Llvm -> False
    Wasm32Wasip3 -> False
