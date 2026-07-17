module Aihc.Cli.Options
  ( Command (..),
    CompileOptions (..),
    InstallErrorFormat (..),
    InstallOptions (..),
    ReplOptions (..),
    parseCommandIO,
    parseCommandPure,
    parserInfo,
  )
where

import Options.Applicative qualified as OA

data Command
  = CmdCompile !CompileOptions
  | CmdInstall !InstallOptions
  | CmdRepl !ReplOptions
  deriving (Eq, Show)

data CompileOptions = CompileOptions
  { compileSourceFile :: !FilePath,
    compileOutputFile :: !(Maybe FilePath),
    compileKeepCore :: !Bool,
    compileKeepGrin :: !Bool,
    compileKeepAsm :: !Bool,
    compileWholeProgram :: !Bool
  }
  deriving (Eq, Show)

newtype ReplOptions = ReplOptions
  { replStoreRoot :: Maybe FilePath
  }
  deriving (Eq, Show)

data InstallOptions = InstallOptions
  { installPackageName :: !String,
    installPackageVersion :: !(Maybe String),
    installStoreRoot :: !(Maybe FilePath),
    installOffline :: !Bool,
    installDryRun :: !Bool,
    installFirstErrorModule :: !Bool,
    installErrorFormat :: !InstallErrorFormat
  }
  deriving (Eq, Show)

data InstallErrorFormat
  = InstallErrorsJson
  | InstallErrorsHuman
  deriving (Eq, Show)

parseCommandIO :: IO Command
parseCommandIO = OA.execParser parserInfo

parseCommandPure :: [String] -> Either String Command
parseCommandPure args =
  case OA.execParserPure OA.defaultPrefs parserInfo args of
    OA.Success command -> Right command
    OA.Failure failure ->
      let (message, _) = OA.renderFailure failure "aihc"
       in Left message
    OA.CompletionInvoked _ -> Left "completion invoked"

parserInfo :: OA.ParserInfo Command
parserInfo =
  OA.info
    (commandParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.header "aihc - command-line interface for the aihc compiler"
    )

commandParser :: OA.Parser Command
commandParser =
  OA.subparser
    ( OA.command
        "compile"
        ( OA.info
            (CmdCompile <$> compileOptionsParser OA.<**> OA.helper)
            (OA.progDesc "Compile a Haskell source file to a native ARM64 executable")
        )
        <> OA.command
          "install"
          ( OA.info
              (CmdInstall <$> installOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Install a Hackage package into the aihc store scaffold")
          )
        <> OA.command
          "repl"
          ( OA.info
              (CmdRepl <$> replOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Start the aihc expression REPL")
          )
    )

compileOptionsParser :: OA.Parser CompileOptions
compileOptionsParser =
  CompileOptions
    <$> OA.strArgument
      ( OA.metavar "SOURCE"
          <> OA.help "Haskell source file containing the main binding"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.short 'o'
              <> OA.long "output"
              <> OA.metavar "FILE"
              <> OA.help "Write the executable to FILE (default: SOURCE without its extension)"
          )
      )
    <*> OA.switch
      ( OA.long "keep-core"
          <> OA.help "Keep the generated System FC core as OUTPUT.core"
      )
    <*> OA.switch
      ( OA.long "keep-grin"
          <> OA.help "Keep the generated GRIN as OUTPUT.grin"
      )
    <*> OA.switch
      ( OA.long "keep-asm"
          <> OA.help "Keep the generated assembly as OUTPUT.s"
      )
    <*> OA.switch
      ( OA.long "whole-program"
          <> OA.help "After incremental module compilation, merge Core units for whole-program DCE and code generation"
      )

replOptionsParser :: OA.Parser ReplOptions
replOptionsParser =
  ReplOptions
    <$> OA.optional
      ( OA.strOption
          ( OA.long "store"
              <> OA.metavar "DIR"
              <> OA.help "Override the aihc store root"
          )
      )

installOptionsParser :: OA.Parser InstallOptions
installOptionsParser =
  InstallOptions
    <$> OA.strArgument
      ( OA.metavar "PACKAGE"
          <> OA.help "Hackage package name"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "version"
              <> OA.metavar "VERSION"
              <> OA.help "Exact Hackage package version"
          )
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "store"
              <> OA.metavar "DIR"
              <> OA.help "Override the aihc store root"
          )
      )
    <*> OA.switch
      ( OA.long "offline"
          <> OA.help "Use only cached package data"
      )
    <*> OA.switch
      ( OA.long "dry-run"
          <> OA.help "Plan the install without writing store artifacts or package cache files"
      )
    <*> OA.switch
      ( OA.long "first-error-module"
          <> OA.help "Only print diagnostics from the module or file that produced the first install error"
      )
    <*> errorFormatParser

errorFormatParser :: OA.Parser InstallErrorFormat
errorFormatParser =
  flagFromSwitch
    <$> OA.switch
      ( OA.long "json-errors"
          <> OA.help "Print install interface diagnostics as JSON instead of human-readable text"
      )
  where
    flagFromSwitch True = InstallErrorsJson
    flagFromSwitch False = InstallErrorsHuman
