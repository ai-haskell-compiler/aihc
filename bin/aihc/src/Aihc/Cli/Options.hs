module Aihc.Cli.Options
  ( Command (..),
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
  = CmdInstall !InstallOptions
  | CmdRepl !ReplOptions
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
