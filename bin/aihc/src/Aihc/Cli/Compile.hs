{-# LANGUAGE OverloadedStrings #-}

-- | Compile a standalone Haskell module through System FC and GRIN to a
-- native Darwin AArch64 executable.
module Aihc.Cli.Compile
  ( CompileError (..),
    compileOutputPath,
    compileSourceToAssembly,
    renderCompileError,
    runCompile,
  )
where

import Aihc.Arm64 (Arm64Error, compileProgram, runtimeSourcePath)
import Aihc.Cli.Options (CompileOptions (..))
import Aihc.Fc (DesugarResult (..), desugarModule)
import Aihc.Grin (lowerProgram)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax (LanguageEdition (Haskell98Edition), effectiveExtensions, headerExtensionSettings, headerLanguageEdition)
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

data CompileError
  = CompileParseError !String
  | CompileFrontendError ![String]
  | CompileArm64Error !Arm64Error
  | CompileClangError !ExitCode !String
  deriving (Eq, Show)

runCompile :: CompileOptions -> IO ()
runCompile options = do
  source <- TIO.readFile (compileSourceFile options)
  assembly <- either (ioError . userError . renderCompileError) pure (compileSourceToAssembly (compileSourceFile options) source)
  let output = compileOutputPath options
  if compileKeepAsm options
    then do
      let assemblyPath = output <> ".s"
      TIO.writeFile assemblyPath assembly
      assemble output assemblyPath
    else withTemporaryDirectory "aihc-compile" $ \directory -> do
      let assemblyPath = directory </> "program.s"
      TIO.writeFile assemblyPath assembly
      assemble output assemblyPath

compileOutputPath :: CompileOptions -> FilePath
compileOutputPath options =
  fromMaybe defaultOutput (compileOutputFile options)
  where
    source = compileSourceFile options
    withoutExtension = dropExtension source
    defaultOutput
      | withoutExtension == source = source <> ".out"
      | otherwise = withoutExtension

compileSourceToAssembly :: FilePath -> Text -> Either CompileError Text
compileSourceToAssembly sourceName source = do
  parsed <-
    case parseModule config source of
      ([], modu) -> Right modu
      (errors, _) -> Left (CompileParseError (show errors))
  let desugared = desugarModule parsed
  if dsSuccess desugared
    then either (Left . CompileArm64Error) Right (compileProgram "main" (lowerProgram (dsProgram desugared)))
    else Left (CompileFrontendError (dsErrors desugared))
  where
    header = readModuleHeaderPragmas source
    language = fromMaybe Haskell98Edition (headerLanguageEdition header)
    config =
      defaultConfig
        { parserSourceName = sourceName,
          parserExtensions = effectiveExtensions language (headerExtensionSettings header)
        }

renderCompileError :: CompileError -> String
renderCompileError compileError =
  case compileError of
    CompileParseError err -> "parse error: " <> err
    CompileFrontendError errors -> "frontend error: " <> unwords errors
    CompileArm64Error err -> "ARM64 code generation error: " <> show err
    CompileClangError exitCode err -> "clang failed (" <> show exitCode <> "): " <> err

assemble :: FilePath -> FilePath -> IO ()
assemble output assemblyPath = do
  runtime <- runtimeSourcePath
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode
      "clang"
      ["-std=c11", "-Wall", "-Wextra", "-Werror", runtime, assemblyPath, "-o", output]
      ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (renderCompileError (CompileClangError exitCode stderr)))

withTemporaryDirectory :: String -> (FilePath -> IO value) -> IO value
withTemporaryDirectory template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary template
      hClose handle
      removeFile path
      createDirectory path
      pure path
